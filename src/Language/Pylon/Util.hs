{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Language.Pylon.Util where

--------------------------------------------------------------------------------

import Prelude             hiding (mapM_)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Error.Class  (MonadError, throwError)
import Data.Foldable              (Foldable, mapM_)

--------------------------------------------------------------------------------

tells :: (MonadWriter w m, Foldable t) => t w -> m ()
tells = mapM_ tell

line :: (MonadWriter String m, Foldable t) => t String -> m ()
line ts = tells ts >> tell "\n"

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

--------------------------------------------------------------------------------

safeIndex :: [a] -> Int -> Maybe a
safeIndex []     _ = Nothing
safeIndex (x:_)  0 = Just x
safeIndex (_:xs) n = safeIndex xs (n - 1)
