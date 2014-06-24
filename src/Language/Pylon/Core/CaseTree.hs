--------------------------------------------------------------------------------
-- |
-- Module      : Language.Pylon.Core.CaseTree
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Left hand side pattern matching to case tree desugaring.
--
-- Based on:
--  * "The Implementation of Functional Programming Languages"
--    Simon Peyton Jones, published by Prentice Hall, 1987
--------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Pylon.Core.CaseTree where
--------------------------------------------------------------------------------
import Language.Pylon.Core.AST
import Language.Pylon.Util.Subst
import Language.Pylon.Util.Name
import Language.Pylon.Util
import Data.Maybe
import Data.List
import Control.Arrow
import Control.Monad
import Control.Applicative
import Control.Monad.State (State, runState, get, modify)

--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------

data Pat 
  = PVar Ident
  | PCon Con [Pat]
  deriving (Eq, Show)

data CaseTree
  = CCase Ident [Clause]
  | CExp  Exp
  deriving (Eq, Show)

data Clause = Clause Con [Ident] CaseTree
  deriving (Eq, Show)

data Equ = Equ [Pat] Exp
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

newtype MatchM a = MatchM { fromMatchM :: State Int a }
  deriving (Functor, Applicative, Monad)

instance MonadName Ident MatchM where
  freshName = MatchM $ do
    n <- get
    modify (\x -> x - 1)
    return $ Ident "_match" n

--------------------------------------------------------------------------------
-- Algorithm
--------------------------------------------------------------------------------

toCaseTree :: Int -> [Equ] -> Exp -> ([Ident], CaseTree)
toCaseTree n qs def = fst $ runState (fromMatchM go) (-1) where
  go = do
    vs <- freshNames n
    ct <- match vs qs $ CExp def
    return (vs, ct)

-- | Elaborates a group of equations matching a list of scrutinized variables.
-- |
-- | In case of an empty pattern match, an equation with no patterns is selected;
-- | if there is no such equation, the default value will be used.
-- |
-- | Otherwise the equations are grouped by their first pattern; then these groups
-- | are elaborated, with the next group serving as the default alternative of
-- | the former. The default alternative of the last group of equations is the
-- | supplied default alternative then.
match :: [Ident] -> [Equ] -> CaseTree -> MatchM CaseTree
match [] qs def = return $ fromMaybe def $ listToMaybe [ CExp e | Equ [] e <- qs ]
match vs qs def = foldr ((=<<) . matchVarCon vs) (return def) $ groupBy (liftPred isVar) qs

-- | Elaborates a group of equations with either all match a variable or all
-- | match a constructor.
matchVarCon :: [Ident] -> [Equ] -> CaseTree -> MatchM CaseTree
matchVarCon vs qs def
  | isVar (head qs) = matchVar vs qs def
  | otherwise       = matchCon vs qs def

-- | Elaborates a group of equations that all match a variable first.
-- | Replaces the pattern variable name with the scrutinized variable in the expression.
matchVar :: [Ident] -> [Equ] -> CaseTree -> MatchM CaseTree
matchVar (v:vs) qs = match vs qs' where
  qs'     = [ Equ ps (sub v u e) | Equ (PVar u : ps) e <- qs ]
  sub a b = subst (singletonSubst a $ EVar b)

-- | Elaborates a group of equations that all match a constructor first.
-- |
-- | Reorders the constructors by grouping equations on the same constructors;
-- | by only moving equations up, semantics are preserved.
matchCon :: [Ident] -> [Equ] -> CaseTree -> MatchM CaseTree
matchCon (v:vs) qs def = do
  let cs = nub [ c | Equ (PCon c _ : _) _ <- qs ]
  cls <- forM cs $ \c -> matchConSame c vs qs def
  return $ CCase v cls

-- | Elaborates a subgroup of equations that match on the given constructor.
-- |
-- | Introduces new variables for each constructor field and lifts the constructor
-- | field patterns to scrutinized patterns; then creates one clause for all the
-- | equations of that constructor.
matchConSame :: Con -> [Ident] -> [Equ] -> CaseTree -> MatchM Clause
matchConSame c vs qs def = do
  let qs' = [ Equ (ps' ++ ps) e | Equ (PCon d ps' : ps) e <- qs, c == d ]
  ws <- freshNames $ conArity c
  ct <- match (ws ++ vs) qs' def
  return $ Clause c ws ct

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

isVar :: Equ -> Bool
isVar (Equ (PVar _ : _) _) = True
isVar _                    = False

isCon :: Equ -> Bool
isCon (Equ (PCon _ _ : _) _) = True
isCon _                      = False

liftPred :: Eq b => (a -> b) -> a -> a -> Bool
liftPred f x y = f x == f y
