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

import Prelude hiding (mapM, sequence)
import Language.Pylon.Core.AST
import Language.Pylon.Core.Eval
import Language.Pylon.Util.Fold
import Control.Arrow
import Control.Monad (join, (>=>))
import Control.Applicative hiding (Const)
import Control.Monad.State.Class  (MonadState, get, put)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State        (State, runState)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import           Data.Traversable (forM, mapM, traverse, sequence)
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

allocScope :: Ident -> Scope Ident
allocScope (ISource n) = do
  i <- get
  _ <- put $ i + 1
  return $ IScoped n i
allocScope n = return n

withScope :: [Ident] -> Scope a -> Scope a
withScope is go = do
  let js = fmap (\(IScoped n u) -> (n, u)) is
  local (\ls -> foldr (uncurry M.insert) ls js) go

findScope :: Ident -> Scope (Maybe Ident)
findScope (ISource name) = do
  mi <- asks $ M.lookup name
  return $ fmap (IScoped name) mi
findScope i = return $ Just i

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
  is <- mapM allocScope [ n | (n, _) <- vs]
  withScope is $ do
    lhs' <- scopeExp lhs
    rhs' <- scopeExp rhs
    ts   <- mapM (scopeExp . snd) vs
    return $ Match (zip is ts) lhs' rhs'

--------------------------------------------------------------------------------
-- Expression Level
--------------------------------------------------------------------------------

scopeExp :: Exp -> Scope Exp
scopeExp = cata alg where
  alg (FVar n) = maybe (toGlobal n) EVar <$> findScope n
  alg e        = traverseBound allocScope e >>= fmap inF . sequence . scoped withScope

toGlobal :: Ident -> Exp
toGlobal (ISource n) = EConst $ CGlobal n
toGlobal n           = EVar n
