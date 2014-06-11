{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Language.Pylon.Util.Name where
--------------------------------------------------------------------------------

import Control.Monad (replicateM)

--------------------------------------------------------------------------------

class Monad m => MonadName n m | m -> n where
  freshName :: m n

freshNames :: MonadName n m => Int -> m [n]
freshNames n = replicateM n freshName