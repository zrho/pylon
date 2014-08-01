-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Pylon.Util.Subst
-- Copyright   :  Lukas Heidemann 2014
-- License     :  BSD
--
-- Maintainer  :  lukasheidemann@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable

-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Language.Pylon.Util.Subst where
-----------------------------------------------------------------------------

import           Data.Monoid
import           Data.Map (Map)
import qualified Data.Map as M

-----------------------------------------------------------------------------

newtype Subst k v = Subst { fromSubst :: Map k v }

class Ord k => Substitutable k v where
  subst :: Subst k v -> v -> v

instance (Show k, Show v) => Show (Subst k v) where
  show = show . fromSubst

instance Functor (Subst k) where
  fmap f = Subst . fmap f . fromSubst

instance Substitutable k v => Monoid (Subst k v) where
  mempty      = Subst mempty
  -- todo: check
  mappend m n = Subst $ fromSubst (fmap (subst n) m) <> fromSubst n

substVar :: Ord k => k -> Subst k v -> Maybe v
substVar k = M.lookup k . fromSubst

mkSubst :: k -> v -> Subst k v
mkSubst k v = Subst $ M.singleton k v
