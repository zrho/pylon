{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Pylon.Util.Generics
-- Copyright   :  Lukas Heidemann 2014
-- License     :  BSD
--
-- Maintainer  :  lukasheidemann@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable

--------------------------------------------------------------------------------
module Language.Pylon.Util.Error where
--------------------------------------------------------------------------------

import Data.Data            (Data, Typeable)
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.Error  (Error, strMsg)

--------------------------------------------------------------------------------
-- Error Contexts
--------------------------------------------------------------------------------

data CtxError c e = CtxError [c] e
  deriving (Eq, Show, Typeable, Data)

instance Error e => Error (CtxError c e) where
  strMsg s = CtxError [] (strMsg s)

throwError' :: MonadError (CtxError c e) m => e -> m a
throwError' e = throwError $ CtxError [] e

errorContext :: MonadError (CtxError c e) m => c -> m a -> m a
errorContext c = flip catchError handleErr where
  handleErr (CtxError cs e) = throwError $ CtxError (c:cs) e