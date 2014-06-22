--------------------------------------------------------------------------------
-- |
-- Module      : Language.Pylon.Core.Check
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Type checker for Pylon Core.
-- Requires an already scoped program.
--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Pylon.Core.Check where
--------------------------------------------------------------------------------

import Language.Pylon.Util
import Language.Pylon.Util.Subst
import Language.Pylon.Core.AST
import Language.Pylon.Core.Eval

import Control.Arrow              
import Control.Monad              (unless, void)
import Control.Monad.Error.Class  (MonadError, throwError, catchError)
import Control.Monad.State.Class  (MonadState, gets, modify)
import Control.Monad.Trans.State  (StateT, runStateT)
import Control.Applicative hiding (Const)

import           Data.Foldable (toList, forM_)
import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Set      (Set)
import qualified Data.Set as S
import           Data.Monoid   ((<>))
import           Data.Maybe    (fromMaybe)

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

newtype Check a = Check { fromCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)

data CheckState = CheckState
  { csLocals   :: Map Ident Type
  , csProgram  :: Program
  } deriving (Eq, Show)

runCheck :: Check a -> Program -> Either String a
runCheck go p = fmap fst $ runStateT (fromCheck go) (CheckState M.empty p)

--------------------------------------------------------------------------------
-- Locals and Program Context
--------------------------------------------------------------------------------

-- | Introduces locals into scope.
withLocals :: [(Ident, Type)] -> Check a -> Check a
withLocals vs go = do
  ws <- gets csLocals
  modify $ \s -> s { csLocals = M.fromList vs <> ws }
  -- check the type of the variables
  forM_ vs $ \(v, t) -> tcExp t
  -- execute in environment
  x <- go
  modify $ \s -> s { csLocals = ws }
  return x

-- | Looks up the type of a local.
lookupLocal :: Ident -> Check Type
lookupLocal i = do
  vs <- gets csLocals
  case M.lookup i vs of
    Just v  -> return v
    Nothing -> throwError $ "Unknown local variable: " ++ show i

-- | Applies a substitution to all locals currently in scope.
substLocals :: Subst Ident Exp -> Check a -> Check a
substLocals u go = do
  vs <- gets csLocals
  modify $ \s -> s { csLocals = fmap (subst u) vs }
  x <- go
  modify $ \s -> s { csLocals = vs }
  return x

-- | Looks up a constructor in the checked program.
lookupCon :: Name -> Check Con
lookupCon name = gets (prData . csProgram) >>= \d -> case M.lookup name d of
  Just con -> return con
  Nothing  -> throwError $ "No such constructor: " ++ name

-- | Looks up a binding in the checked program.
lookupBind :: Name -> Check Bind
lookupBind name = gets (prBind . csProgram) >>= \b -> case M.lookup name b of
  Just bnd -> return bnd
  Nothing  -> throwError $ "No such binding: " ++ name

--------------------------------------------------------------------------------
-- Programs
--------------------------------------------------------------------------------

tcProgram :: Check ()
tcProgram = do
  p <- gets csProgram
  mapM_ tcBind $ M.toList $ prBind p

tcBind :: (Name, Bind) -> Check ()
tcBind (name, Bind e t) = do
  et <- tcExp e
  ensureEq (nf t) (nf et) $ "Type mismatch in binding: " ++ name

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

tcExp :: Exp -> Check Type
tcExp (EConst c )     = tcConst c
tcExp (EApp f x )     = tcApp f x
tcExp (ELam (i, t) e) = tcLam i t e
tcExp (EPi (i, t) e ) = tcPi i t e
tcExp (ELet bs e)     = tcLet bs e 
tcExp (ECase as d e)  = tcCase as d e
tcExp (EVar i)        = tcVar i
tcExp (EPrim po xs)   = tcPrim po xs

tcExpNF :: Exp -> Check Type
tcExpNF = fmap nf . tcExp

tcConst :: Const -> Check Type 
tcConst (CLit    lit ) = return $ litType lit
tcConst (CCon    name) = conType <$> lookupCon name
tcConst (CGlobal name) = bndType <$> lookupBind name
tcConst CUniv          = return $ EConst CUniv
tcConst CPrimUniv      = return $ EConst CUniv
tcConst (CPrim p     ) = return $ EConst CPrimUniv

tcApp :: Exp -> Exp -> Check Type
tcApp f x = tcExpNF f >>= \ft -> case ft of
  EPi (i, tx) rt -> do
    xt <- tcExpNF x
    ensureEq (nf tx) xt $ "Bad argument type in " ++ show (EApp f x)
    return $ subst (singletonSubst i x) rt
  _ -> throwError "Application to non-function."

tcLam :: Ident -> Type -> Exp -> Check Type
tcLam i t e = do
  rt <- withLocals [(i, t)] $ tcExp e
  return $ EPi (i, t) rt

tcPi :: Ident -> Type -> Exp -> Check Type
tcPi i t e = do
  _ <- withLocals [(i, t)] $ tcExp e
  return $ EConst CUniv

tcLet :: [(Ident, Exp, Type)] -> Exp -> Check Type
tcLet bs e = do
  let vs = fmap (\(i, _, t) -> (i, t)) bs
  withLocals vs $ do
    forM_ bs $ \(_, x, t) -> tcExp x
    tcExp e

tcVar :: Ident -> Check Type
tcVar = lookupLocal

--------------------------------------------------------------------------------
-- Primitive and Literals
--------------------------------------------------------------------------------

-- | Check primitive application.
-- todo: add IO
tcPrim :: PrimOp -> [Exp] -> Check Type
tcPrim p xs = do
  let (as, r) = primType p
  unless (length as == length xs) $ throwError "Under/oversaturated primitive application."
  forM_ (zip as xs) $ \(a, x) -> do
    xt <- fmap nf $ tcExp x
    ensureEq (EConst $ CPrim a) xt $ "Mismatch in primitive type."
  return $ EConst $ CPrim r

primType :: PrimOp -> ([Prim], Prim)
primType p = case p of
  PPlus p         -> binOp p
  PMult p         -> binOp p
  PDiv p          -> binOp p
  PMinus p        -> binOp p
  PEq p           -> binOp p
  PLt p           -> binOp p
  PLte p          -> binOp p
  PGt p           -> binOp p
  PGte p          -> binOp p
  PForeign _ ps p -> (ps, p)
  where binOp p = ([p, p], p)

litType :: Lit -> Type
litType (LInt _) = EConst $ CPrim PInt

--------------------------------------------------------------------------------
-- Case Expressions
--------------------------------------------------------------------------------

-- | Check case expressions.
tcCase :: Alts Exp -> (Ident, Exp) -> Exp -> Check Type
tcCase (AAlts as) d e = tcCaseAlg as d e
tcCase (PAlts as) d e = tcCasePrim as d e

-- | Check algebraic cases.
tcCaseAlg :: [AAlt Exp] -> (Ident, Exp) -> Exp -> Check Type
tcCaseAlg as (di, de) e = do
  -- type of the scrutinee
  et <- tcExp e
  -- type of the default alternative
  dt <- fmap nf $ withLocals [(di, et)] $ tcExp de
  -- type check alternatives
  forM_ as $ \(AAlt c vs a) -> do
    con <- lookupCon c
    -- apply the constructor type to the variables
    -- tc then is the expected type of the scrutinee,
    -- vst the types of the variables
    let (tc, vst) = apply (conType con) $ fmap EVar vs
    let ws = zip vs vst
    -- unify the actual scrutinee type with tc
    u <- liftEither $ runUnify $ unify tc et
    -- now check the alternative's expression
    at <- withLocals ws $ substLocals u $ do
      ls <- gets csLocals
      tcExp $ subst u a
    ensureEq (subst u dt) (nf at) $ "Case alternatives with different result types."
  return dt

-- | Check primitive cases.
tcCasePrim :: [PAlt Exp] -> (Ident, Exp) -> Exp -> Check Type
tcCasePrim as (di, de) e = do
  et <- tcExp e
  dt <- fmap nf $ withLocals [(di, de)] $ tcExp de
  forM_ as $ \(PAlt l a) -> do
    ensureEq et (litType l) $ "Type mismatch in literal pattern."
    at <- tcExp a
    ensureEq dt (nf at) $ "Case alternatives with different result types."
  return dt

--------------------------------------------------------------------------------

ensureEq :: Exp -> Exp -> String -> Check ()
ensureEq ex ac msg = case alphaEq ex ac of
  Right _ -> return ()
  Left e  -> throwError $ unlines
    [ msg, "Expected: " ++ show ex, "Actual: " ++ show ac
    , "Unifier message: " ++ show e ]