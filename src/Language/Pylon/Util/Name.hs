{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Language.Pylon.Util.Name where
--------------------------------------------------------------------------------

import Control.Monad (replicateM)
import Control.Concurrent.Supply

--------------------------------------------------------------------------------

class Monad m => MonadName n m | m -> n where
  freshName :: m n

freshNames :: MonadName n m => Int -> m [n]
freshNames n = replicateM n freshName

--------------------------------------------------------------------------------

class Monad m => MonadSupply m where
  getSupply :: m Supply
  putSupply :: Supply -> m ()

supplyId :: MonadSupply m => m Int
supplyId = do
  s <- getSupply
  let (n, s') = freshId s
  putSupply s'
  return n

supplySplit :: MonadSupply m => m Supply
supplySplit = do
  s <- getSupply
  let (p, q) = splitSupply s
  putSupply p
  return q

withSupply :: MonadSupply m => (Supply -> m a) -> m a
withSupply go = supplySplit >>= go
