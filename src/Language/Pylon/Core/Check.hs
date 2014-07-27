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
import Language.Pylon.Core.Monad
import Language.Pylon.Core.Eval

import Control.Arrow
import Control.Monad              (unless, void)
import Control.Monad.Error.Class  (MonadError, throwError, catchError)
import Control.Monad.State.Class  (MonadState, gets, modify)
import Control.Monad.Trans.State  (StateT, runStateT)
import Control.Applicative hiding (Const)

import           Data.Foldable (toList, forM_, foldMap)
import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Set      (Set)
import qualified Data.Set as S
import           Data.Monoid   ((<>))
import           Data.Maybe    (fromMaybe)
import           Data.List     (sort)

import Debug.Trace

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

newtype Check a = Check { fromCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)

data CheckState = CheckState
  { csLocals   :: Map Ident Type
  , csProgram  :: Program
  } deriving (Eq, Show)

instance MonadProgram Check where
  getProgram = gets csProgram

runCheck :: Check a -> Program -> Map Ident Type -> Either String a
runCheck go p loc = fmap fst $ runStateT (fromCheck go) $ CheckState loc p

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

--------------------------------------------------------------------------------
-- Programs
--------------------------------------------------------------------------------

tcProgram :: Check ()
tcProgram = do
  p <- gets csProgram
  mapM_ tcBind $ M.toList $ prBind p

tcBind :: (Name, Bind) -> Check ()
tcBind (name, Bind Nothing t) = return ()
tcBind (name, Bind (Just ms) t) = do
  -- todo check lhs count
  mapM_ (tcMatch name) ms

--------------------------------------------------------------------------------
-- Matches
--------------------------------------------------------------------------------

tcMatch :: Name -> Match -> Check ()
tcMatch name (Match vs lhs rhs) =
  withLocals vs $ do
    unless (isLhsForm name lhs) $ throwError $ "Illegal left hand side form in binding: " ++ name
    lhst <- tcExp lhs
    rhst <- tcExp rhs
    ensureEq (nf lhst) (nf rhst) $ "Type mismatch in binding: " ++ name

isLhsForm :: Name -> Exp -> Bool
isLhsForm n (EApp f x)           = isLhsForm n f
isLhsForm n (EConst (CGlobal m)) = n == m
isLhsForm _ _                    = False

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

tcExp :: Exp -> Check Type
tcExp (EConst c )    = tcConst c
tcExp (EApp f x )    = tcApp f x
tcExp (ELam i t e)   = tcLam i t e
tcExp (EPi i t e )   = tcPi i t e
tcExp (ELet i t b e) = tcLet i t b e
tcExp (EVar i)       = tcVar i
tcExp (EPrim po xs)  = tcPrim po xs
-- todo: holes

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
  EPi i tx rt -> do
    xt <- tcExpNF x
    ensureEq (nf tx) xt $ "Bad argument type in " ++ show (EApp f x)
    return $ subst (singletonSubst i x) rt
  _ -> throwError "Application to non-function."

tcLam :: Ident -> Type -> Exp -> Check Type
tcLam i t e = do
  rt <- withLocals [(i, t)] $ tcExp e
  return $ EPi i t rt

tcPi :: Ident -> Type -> Exp -> Check Type
tcPi i t e = do
  _ <- withLocals [(i, t)] $ tcExp e
  return $ EConst CUniv

tcLet :: Ident -> Type -> Exp -> Exp -> Check Type
tcLet i t b e = withLocals [(i, t)] $ do
  bt <- tcExp b
  ensureEq (nf t) (nf bt) $ "Bad typed let binding in: " ++ show (ELet i t b e)
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

ensureEq :: Exp -> Exp -> String -> Check ()
ensureEq ex ac msg
  | alphaEq ex ac = return ()
  | otherwise     = throwError $ unlines
    [ msg, "Expected: " ++ show ex, "Actual: " ++ show ac ]
