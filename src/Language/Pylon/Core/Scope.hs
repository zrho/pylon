--------------------------------------------------------------------------------
-- |
-- Module      : Pylon.Core.AST
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Traverses a Pylon program, introduces unique numeric indices for each
-- local variable (with multiple occurences of the same variable with regards
-- to scoping rules being assigned the same index) and raises out-of-scope
-- variables to global constants.
--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
module Language.Pylon.Core.Scope where
--------------------------------------------------------------------------------

import Prelude hiding (mapM)
import Language.Pylon.Core.AST
import Control.Applicative hiding (Const)
import Control.Monad.State.Class  (MonadState, get, put)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State        (State, runState)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import           Data.Traversable (forM, mapM)
import           Data.Map         (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

newtype Scope a = Scope { fromScope :: ReaderT (Map Name Int) (State Int) a }
  deriving (Functor, Applicative, Monad, MonadState Int, MonadReader (Map Name Int))

runScope :: Scope a -> a
runScope go = fst $ runState (runReaderT (fromScope go) M.empty) 0

allocScope :: Name -> Scope Ident
allocScope name = do
  i <- get
  _ <- put $ i + 1
  return $ Ident name i

withScope :: [Ident] -> Scope a -> Scope a
withScope is go = do
  let js = fmap (\(Ident n u) -> (n, u)) is
  local (\ls -> foldr (uncurry M.insert) ls js) go

findScope :: Name -> Scope (Maybe Ident)
findScope name = do
  mi <- asks $ M.lookup name
  return $ fmap (Ident name) mi

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

scopeProgram :: Program -> Scope Program
scopeProgram (Program cs bs) = Program
  <$> mapM scopeCon cs
  <*> mapM scopeBind bs

scopeBind :: Bind -> Scope Bind
scopeBind (Bind e t) = Bind <$> scopeExp e <*> scopeExp t

scopeCon :: Con -> Scope Con
scopeCon (Con i t) = Con i <$> scopeExp t

--------------------------------------------------------------------------------
-- Expression Level
--------------------------------------------------------------------------------

scopeExp :: Exp -> Scope Exp
scopeExp (EConst c)         = scopeConst c
scopeExp (EApp f x)         = scopeApp f x
scopeExp (ELam b e)         = scopeLam b e
scopeExp (EPi b e)          = scopePi b e
scopeExp (ELet bs e)        = scopeLet bs e
scopeExp (ECase ps d e)     = scopeCase ps d e
scopeExp (EVar (Ident n _)) = scopeVar n

scopeLam :: (Ident, Type) -> Exp -> Scope Exp
scopeLam (Ident n _, t) e = do
  i  <- allocScope n
  t' <- scopeExp t
  e' <- withScope [i] $ scopeExp e
  return $ ELam (i, t') e'

scopePi :: (Ident, Type) -> Exp -> Scope Exp
scopePi (Ident n _, t) e = do
  i  <- allocScope n
  t' <- scopeExp t
  e' <- withScope [i] $ scopeExp e
  return $ EPi (i, t') e'

scopeLet :: [(Ident, Exp, Type)] -> Exp -> Scope Exp
scopeLet bs e = do
  vs   <- forM bs $ \(i, _, _) -> allocScope $ iName i
  withScope vs $ do
    bs'  <- forM (zip vs bs) $ \(i, (_, e, t)) -> do
      e' <- scopeExp e
      t' <- scopeExp t
      return (i, e', t')
    ELet bs' <$> scopeExp e

scopeCase :: Alts Exp -> (Ident, Exp) -> Exp -> Scope Exp
scopeCase as (di, de) e = do
  di'   <- allocScope $ iName di
  de'   <- withScope [di] $ scopeExp de
  as'   <- scopeAlts as
  ECase as' (di', de') <$> scopeExp e

scopeAlts :: Alts Exp -> Scope (Alts Exp)
scopeAlts (AAlts as) = fmap AAlts $ forM as $ \(AAlt con vs e) -> do
  ws  <- mapM (allocScope . iName) vs
  e' <- withScope ws $ scopeExp e
  return (AAlt con ws e')
scopeAlts (PAlts as) = fmap PAlts $ forM as $ \(PAlt l e) -> 
  PAlt l <$> scopeExp e

scopeConst :: Const -> Scope Exp
scopeConst = return . EConst

scopeApp :: Exp -> Exp -> Scope Exp
scopeApp f x = EApp <$> scopeExp f <*> scopeExp x

scopeVar :: Name -> Scope Exp
scopeVar n = maybe (EConst $ CGlobal n) EVar <$> findScope n
