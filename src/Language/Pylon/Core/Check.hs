{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
module Language.Pylon.Core.Check where
--------------------------------------------------------------------------------

import Language.Pylon.Util
import Language.Pylon.Core.AST
import Language.Pylon.Core.Eval

import Control.Monad              (unless)
import Control.Monad.Error.Class  (MonadError, throwError)
import Control.Monad.State.Class  (MonadState, gets, modify)
import Control.Monad.Trans.State  (StateT, runStateT)
import Control.Applicative hiding (Const)

import           Data.Foldable (toList)
import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Monoid   ((<>))

import Debug.Trace

traceM :: (Monad m) => String -> m ()
traceM msg = traceShow msg $ return ()

--------------------------------------------------------------------------------

newtype Check a = Check { fromCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)

data CheckState = CheckState
  { csLocals  :: Map Ident Type
  , csProgram :: Program
  } deriving (Eq, Show)

runCheck :: Check a -> Program -> Either String a
runCheck go p = fmap fst $ runStateT (fromCheck go) (CheckState M.empty p)

--------------------------------------------------------------------------------

withLocals :: [(Ident, Type)] -> Check a -> Check a
withLocals vs go = do
  ws <- gets csLocals
  modify $ \s -> s { csLocals = M.fromList vs <> ws }
  x <- go
  modify $ \s -> s { csLocals = ws }
  return x

lookupLocal :: Ident -> Check Type
lookupLocal i = do
  vs <- gets csLocals
  case M.lookup i vs of
    Just v  -> return v
    Nothing -> throwError $ "Unknown local variable: " ++ show i

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
    ensureEq (nf tx) xt "Bad argument type."
    return $ subst i xt rt
  _ -> throwError "Application to non-function."

tcLam :: Ident -> Type -> Exp -> Check Type
tcLam i t e = do
  _  <- tcExp t -- check t
  rt <- withLocals [(i, t)] $ tcExp e
  return $ EPi (i, t) rt

tcPi :: Ident -> Type -> Exp -> Check Type
tcPi i t e = do
  _ <- tcExp t -- check t
  _ <- withLocals [(i, t)] $ tcExp e
  return $ EConst CUniv

tcLet :: [(Ident, Exp, Type)] -> Exp -> Check Type
tcLet bs e = error "todo"

tcCase :: [(Pat, Exp)] -> (Ident, Exp) -> Exp -> Check Type
tcCase as d e = error "todo"

tcVar :: Ident -> Check Type
tcVar = lookupLocal

--------------------------------------------------------------------------------

-- todo
litType :: Lit -> Type
litType (LInt _) = EConst $ CGlobal "Pylon.Prim.Int"

--------------------------------------------------------------------------------

ensureEq :: (Eq a, Show a) => a -> a -> String -> Check ()
ensureEq ex ac msg = unless (ex == ac) $ throwError $ unlines
  [ msg, "Expected: " ++ show ex, "Actual: " ++ show ac ]