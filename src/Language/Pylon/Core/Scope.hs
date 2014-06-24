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
import Language.Pylon.Util.Fold
import Control.Arrow
import Control.Applicative hiding (Const)
import Control.Monad.State.Class  (MonadState, get, put)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State        (State, runState)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import           Data.Traversable (forM, mapM)
import           Data.Foldable    (foldMap, fold)
import           Data.Maybe       (fromMaybe)
import           Data.Map         (Map)
import qualified Data.Map as M
import qualified Data.Set as S

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
scopeBind (Bind ms t) = Bind
  <$> (mapM . mapM) scopeMatch ms
  <*> scopeExp t

scopeCon :: Con -> Scope Con
scopeCon (Con i t) = Con i <$> scopeExp t

--------------------------------------------------------------------------------
-- Matches and Patterns
--------------------------------------------------------------------------------

scopeMatch :: Match -> Scope Match
scopeMatch (Match vs lhs rhs) = do
  is <- mapM allocScope $ fmap (iName . fst) vs
  withScope is $ do
    lhs' <- scopeExp lhs
    rhs' <- scopeExp rhs
    ts   <- mapM (scopeExp . snd) vs
    return $ Match (zip is ts) lhs' rhs'

{-patVarNames :: Pat -> S.Set Name
patVarNames = cata alg where
  alg (PFVar v) = S.singleton (iName v)
  alg p         = fold p

patVarScope :: [Ident] -> Pat -> Pat
patVarScope is = cata alg where
  alg (PFVar v) = maybe (PVar v) PVar $ M.lookup (iName v) isMap
  alg p         = inF p
  isMap         = M.fromList $ fmap (iName &&& id) is-}

--------------------------------------------------------------------------------
-- Expression Level
--------------------------------------------------------------------------------

scopeExp :: Exp -> Scope Exp
scopeExp (EConst c)         = scopeConst c
scopeExp (EApp f x)         = scopeApp f x
scopeExp (ELam b e)         = scopeLam b e
scopeExp (EPi b e)          = scopePi b e
scopeExp (ELet bs e)        = scopeLet bs e
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

scopeConst :: Const -> Scope Exp
scopeConst = return . EConst

scopeApp :: Exp -> Exp -> Scope Exp
scopeApp f x = EApp <$> scopeExp f <*> scopeExp x

scopeVar :: Name -> Scope Exp
scopeVar n = maybe (EConst $ CGlobal n) EVar <$> findScope n
