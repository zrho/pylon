{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Language.Pylon.Util where

--------------------------------------------------------------------------------

import Prelude             hiding (mapM_)
import Control.Monad.RWS          (RWST, evalRWST)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Except       (Except, ExceptT, MonadError, throwError, runExcept)
import Control.Monad.Error.Class  (Error, strMsg)
import Data.Foldable              (Foldable, mapM_)
import Data.List                  (nub)

--------------------------------------------------------------------------------

type RWSE r w s e a = RWST r w s (Except e) a

runRWSE :: RWSE r w s e a -> r -> s -> Either e a
runRWSE go r s = runExcept $ fmap fst $ evalRWST go r s

tells :: (MonadWriter w m, Foldable t) => t w -> m ()
tells = mapM_ tell

line :: (MonadWriter String m, Foldable t) => t String -> m ()
line ts = tells ts >> tell "\n"

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

liftEitherStr :: (Error e, MonadError e m) => Either String a -> m a
liftEitherStr = either (throwError . strMsg) return

--------------------------------------------------------------------------------

safeIndex :: [a] -> Int -> Maybe a
safeIndex []     _ = Nothing
safeIndex (x:_)  0 = Just x
safeIndex (_:xs) n = safeIndex xs (n - 1)

indexOf :: Eq a => a -> [a] -> Maybe Int
indexOf x [] = Nothing
indexOf x (y:ys)
  | x == y    = Just 0
  | otherwise = fmap (+1) $ indexOf x ys

unique :: Eq a => [a] -> Bool
unique xs = length xs == length (nub xs)

pairwise :: (a -> a -> Bool) -> [a] -> Bool
pairwise _ []     = True
pairwise _ [_]    = True
pairwise p (x:xs) = all (p x) xs && pairwise p xs