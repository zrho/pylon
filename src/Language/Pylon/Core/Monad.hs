--------------------------------------------------------------------------------
-- |
-- Module      : Pylon.Core.Language.Monad
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--------------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Language.Pylon.Core.Monad where
--------------------------------------------------------------------------------

import Language.Pylon.Core.AST
import Language.Pylon.Util.Name
import Control.Applicative
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Error.Class (MonadError, Error, throwError, strMsg)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Monads with a program
--------------------------------------------------------------------------------

class MonadProgram m where
  getProgram :: m Program

lookupBind :: (Error e, MonadError e m, MonadProgram m) => Name -> m Bind
lookupBind name = getProgram >>= \p -> case M.lookup name (prBind p) of
  Nothing -> throwError $ strMsg $ "No such binding: " ++ name
  Just x  -> return x

lookupCon :: (Error e, MonadError e m, MonadProgram m) => Name -> m Con
lookupCon name = getProgram >>= \p -> case M.lookup name (prData p) of
  Nothing -> throwError $ strMsg $ "No such constructor: " ++ name
  Just x  -> return x
