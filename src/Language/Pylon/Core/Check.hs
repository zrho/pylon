{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
module Language.Pylon.Core.Check where
--------------------------------------------------------------------------------

import Language.Pylon.Util
import Language.Pylon.Util.Subst
import Language.Pylon.Core.AST
import Language.Pylon.Core.Eval

import Control.Arrow              
import Control.Monad              (unless)
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

newtype Check a = Check { fromCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)

data CheckState = CheckState
  { csLocals   :: Map Ident Type
  , csProgram  :: Program
  } deriving (Eq, Show)

runCheck :: Check a -> Program -> Either String a
runCheck go p = fmap fst $ runStateT (fromCheck go) (CheckState M.empty p S.empty)

--------------------------------------------------------------------------------

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

lookupLocal :: Ident -> Check Type
lookupLocal i = do
  vs <- gets csLocals
  case M.lookup i vs of
    Just v  -> return v
    Nothing -> throwError $ "Unknown local variable: " ++ show i

substLocals :: Subst Ident Exp -> Check a -> Check a
substLocals u go = do
  vs <- gets csLocals
  modify $ \s -> s { csLocals = fmap (subst u) vs }
  x <- go
  modify $ \s -> s { csLocals = vs }
  return x

lookupCon :: Name -> Check Con
lookupCon name = gets (prData . csProgram) >>= \d -> case M.lookup name d of
  Just con -> return con
  Nothing  -> throwError $ "No such constructor: " ++ name

lookupBind :: Name -> Check Bind
lookupBind name = gets (prBind . csProgram) >>= \b -> case M.lookup name b of
  Just bnd -> return bnd
  Nothing  -> throwError $ "No such binding: " ++ name

--------------------------------------------------------------------------------

tcProgram :: Check ()
tcProgram = do
  p <- gets csProgram
  mapM_ tcBind $ M.toList $ prBind p

tcBind :: (Name, Bind) -> Check ()
tcBind (name, Bind e t) = do
  et <- tcExp e
  ensureEq (nf t) (nf et) $ "Type mismatch in binding: " ++ name
  return ()

--------------------------------------------------------------------------------

tcExp :: Exp -> Check Type
tcExp (EConst c )     = tcConst c
tcExp (EApp f x )     = tcApp f x
tcExp (ELam (i, t) e) = tcLam i t e
tcExp (EPi (i, t) e ) = tcPi i t e
tcExp (ELet bs e)     = tcLet bs e 
tcExp (ECase as d e)  = tcCase as d e
tcExp (EVar i)        = tcVar i

tcExpNF :: Exp -> Check Type
tcExpNF = fmap nf . tcExp

--------------------------------------------------------------------------------

tcConst :: Const -> Check Type 
tcConst (CLit    lit ) = return $ litType lit
tcConst (CCon    name) = conType <$> lookupCon name
tcConst (CGlobal name) = bndType <$> lookupBind name
tcConst CUniv          = return $ EConst CUniv

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

tcCase :: [(Pat, Exp)] -> (Ident, Exp) -> Exp -> Check Type
tcCase as (di, de) e = do
  et <- tcExp e
  dt <- fmap nf $ withLocals [(di, et)] $ tcExp de
  forM_ as $ \(PCon c vs, a) -> do
    con <- lookupCon c
    let (tc, vst) = apply (conType con) $ fmap EVar vs
    let ws = zip vs vst
    u <- liftEither $ runUnify $ unify tc et
    at <- withLocals ws $ substLocals u $ do
      ls <- gets csLocals
      tcExp $ subst u a
    ensureEq (subst u dt) (nf at) $ "Case alternatives with different result types."
  return dt

tcVar :: Ident -> Check Type
tcVar = lookupLocal

--------------------------------------------------------------------------------

-- todo
litType :: Lit -> Type
litType (LInt _) = EConst $ CGlobal "Pylon.Prim.Int"

--------------------------------------------------------------------------------

ensureEq :: Exp -> Exp -> String -> Check ()
ensureEq ex ac msg = case alphaEq ex ac of
  Right _ -> return ()
  Left e  -> throwError $ unlines
    [ msg, "Expected: " ++ show ex, "Actual: " ++ show ac
    , "Unifier message: " ++ show e ]