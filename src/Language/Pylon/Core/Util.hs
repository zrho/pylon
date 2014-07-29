--------------------------------------------------------------------------------
-- |
-- Module      : Language.Pylon.Core.Util
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Utility functions to work with Pylon expressions.
--------------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}
module Language.Pylon.Core.Util where
--------------------------------------------------------------------------------

import Data.Generics.Uniplate.Data
import Language.Pylon.Core.AST
import qualified Data.Set as S
import           Data.Set (Set)

--------------------------------------------------------------------------------
-- * Binders and Variables 
--------------------------------------------------------------------------------

-- | Opens the body of an abstraction with the given term.
-- | The input expression must NOT include the abstraction.
open :: Exp -> Exp -> Exp
open s ee = go 0 ee where
  go j (ELocal i) | i == j = s
  go j (EBind b e)         = EBind (descendBi (go j) b) $ go (j + 1) e
  go j e                   = descend (go j) e

-- | Closes an abstraction that binds the given identifier.
-- | The output expression does NOT include the abstraction.
close :: Ident -> Exp -> Exp
close n ee = go 0 ee where
  go j (EFree m) | n == m = ELocal j
  go j (EBind b e)        = EBind (descendBi (go j) b) $ go (j + 1) e
  go j e                  = descend (go j) e

-- | Checks whether the given expression is locally closed.
-- | An expression is locally closed, if for every bound variable, there
-- | is a binder in the expression.
locallyClosed :: Exp -> Bool
locallyClosed ee = go 0 ee where
  go j (ELocal i)  = i <= j
  go j (EBind b e) = (all (go j) $ childrenBi b) && go (j + 1) e
  go j e           = all (go j) $ children e

-- | Free variables in the expression.
-- | Assumption: The expression is locally closed.
freeVars :: Exp -> Set Ident
freeVars ee = S.fromList [ v | EFree v <- universe ee ]

--------------------------------------------------------------------------------
-- Normal Forms
--------------------------------------------------------------------------------

-- | Computes the head normal form of an expression.
hnf :: Exp -> Exp
hnf ee = go ee [] where
  go (EApp f x) as     = go f (x:as)
  go (ELam t e) []     = ELam (hnf t) (hnf e)
  go (ELam _ e) (a:as) = go (open a e) as
  go e          as     = foldl EApp e $ fmap hnf as