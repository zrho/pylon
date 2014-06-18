-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Pylon.Util.Fold
-- Copyright   :  Lukas Heidemann 2014
-- License     :  BSD
--
-- Maintainer  :  lukasheidemann@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable
--
-- This package provides fold and their dual unfold operations on recursive
-- data structures, defined by the fixpoint of a functor.
--
-- Besides the default fixpoint type Fix, an instance of the Fixpoint typeclass
-- can be defined for own fixpoint types.

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TupleSections, FlexibleInstances #-}
module Language.Pylon.Util.Fold where

import Prelude              hiding (sequence, mapM)
import Control.Applicative
import Control.Arrow
import Control.Monad        hiding (mapM, sequence)
import Data.Traversable

--------------------------------------------------------------------------------
-- * Fixpoints
--------------------------------------------------------------------------------

-- | Type Class for functor fixpoints.
class Functor f => Fixpoint f t | t -> f where
  inF  :: f t -> t
  outF :: t   -> f t

-- | Simple functorial fixpoint.
newtype Fix f = Fix { unFix :: f (Fix f) }

instance Functor f => Fixpoint f (Fix f) where
  inF  = Fix
  outF = unFix

--------------------------------------------------------------------------------
-- * Folds, Unfolds and Refolds
--------------------------------------------------------------------------------

-- | Catamorphism: Fold.
cata :: Fixpoint f t => (f a -> a) -> t -> a
cata alg = alg . fmap (cata alg) . outF

-- | Paramorphism: Fold with original object.
para :: Fixpoint f t => (f (a, t) -> a) -> t -> a
para alg = alg . fmap (para alg &&& id) . outF

-- | Anamorphism: Unfold.
ana :: Fixpoint f t => (a -> f a) -> a -> t
ana coalg = inF . fmap (ana coalg) . coalg

-- | Apomorphism: Unfold that may deliver a result before completion.
apo :: Fixpoint f t => (a -> f (Either a t)) -> a -> t
apo coalg = inF . fmap (apo coalg ||| id) . coalg

-- | Hylomorphism: Composition of fold and unfold.
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg

--------------------------------------------------------------------------------
-- * Effects
--------------------------------------------------------------------------------

-- | Catamorphism in a monad.
cataM :: (Fixpoint f t, Monad m, Traversable f) => (f a -> m a) -> t -> m a
cataM alg = cata $ sequence >=> alg

-- | Paramorphism in a monad.
paraM :: (Fixpoint f t, Monad m, Traversable f) => (f (a, t) -> m a) -> t -> m a
paraM alg = para $ mapM fstM >=> alg

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

fstM :: Monad m => (m a, b) -> m (a, b)
fstM (r, t) = liftM (,t) r