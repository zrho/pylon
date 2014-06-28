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
{-# LANGUAGE PatternSynonyms #-}
module Language.Pylon.Core.CaseTree
  ( Pat (..)
  , CaseTree (..)
  , Clause (..)
  , toCaseTree
  ) where
--------------------------------------------------------------------------------
import Language.Pylon.Core.AST
import Language.Pylon.Core.Monad
import Language.Pylon.Util.Subst
import Language.Pylon.Util.Name
import Language.Pylon.Util
import Data.Maybe
import Data.List
import Control.Arrow
import Control.Monad
import Control.Applicative
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Error.Class (MonadError, throwError)

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

-- | Compiles a list of matches and a default expression into a case tree
-- | that scrutinizes a list of variables.
toCaseTree ::  (MonadError String m, MonadProgram m) => [Match] -> Exp -> m (CaseTree, [Ident])
toCaseTree ms def = runCT $ do
  let n = fromMaybe 0 $ fmap matchArity $ listToMaybe ms
  qs <- mapM matchToEqu ms
  vs <- freshNames n
  ct <- match vs qs (CExp def)
  return (ct, vs)

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

newtype CT a = CT { fromCT :: StateT CTState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState CTState, MonadError String)

data CTState = CTState
  { ctProgram :: Program
  , ctNames   :: Int
  } deriving (Eq, Show)

instance MonadProgram CT where
  getProgram = gets ctProgram

instance MonadName Ident CT where
  freshName = do
    n <- gets ctNames
    modify $ \s -> s { ctNames = n + 1 }
    return $ IGen "CaseTree" n

runCT :: (MonadError String m, MonadProgram m) => CT a -> m a
runCT go = do
  p <- getProgram
  liftEither $ fmap fst $ runStateT (fromCT go) $ CTState p 0

--------------------------------------------------------------------------------
-- Equations to Case Trees
--------------------------------------------------------------------------------

-- | Elaborates a group of equations matching a list of scrutinized variables.
-- |
-- | In case of an empty pattern match, an equation with no patterns is selected;
-- | if there is no such equation, the default value will be used.
-- |
-- | Otherwise the equations are grouped by their first pattern; then these groups
-- | are elaborated, with the next group serving as the default alternative of
-- | the former. The default alternative of the last group of equations is the
-- | supplied default alternative then.
match :: [Ident] -> [Equ] -> CaseTree -> CT CaseTree
match [] qs def = return $ fromMaybe def $ listToMaybe [ CExp e | Equ [] e <- qs ]
match vs qs def = foldr ((=<<) . matchVarCon vs) (return def) $ groupBy (liftPred isVar) qs

-- | Elaborates a group of equations with either all match a variable or all
-- | match a constructor.
matchVarCon :: [Ident] -> [Equ] -> CaseTree -> CT CaseTree
matchVarCon vs qs def
  | isVar (head qs) = matchVar vs qs def
  | otherwise       = matchCon vs qs def

-- | Elaborates a group of equations that all match a variable first.
-- | Replaces the pattern variable name with the scrutinized variable in the expression.
matchVar :: [Ident] -> [Equ] -> CaseTree -> CT CaseTree
matchVar (v:vs) qs = match vs qs' where
  qs'     = [ Equ ps (sub v u e) | Equ (PVar u : ps) e <- qs ]
  sub a b = subst (singletonSubst a $ EVar b)

-- | Elaborates a group of equations that all match a constructor first.
-- |
-- | Reorders the constructors by grouping equations on the same constructors;
-- | by only moving equations up, semantics are preserved.
matchCon :: [Ident] -> [Equ] -> CaseTree -> CT CaseTree
matchCon (v:vs) qs def = do
  let cs = nub [ c | Equ (PCon c _ : _) _ <- qs ]
  cls <- forM cs $ \c -> matchConSame c vs qs def
  return $ CCase v cls

-- | Elaborates a subgroup of equations that match on the given constructor.
-- |
-- | Introduces new variables for each constructor field and lifts the constructor
-- | field patterns to scrutinized patterns; then creates one clause for all the
-- | equations of that constructor.
matchConSame :: Con -> [Ident] -> [Equ] -> CaseTree -> CT Clause
matchConSame c vs qs def = do
  let qs' = [ Equ (ps' ++ ps) e | Equ (PCon d ps' : ps) e <- qs, c == d ]
  ws <- freshNames $ conArity c
  ct <- match (ws ++ vs) qs' def
  return $ Clause c ws ct

--------------------------------------------------------------------------------
-- Matches to Equations
--------------------------------------------------------------------------------

matchToEqu :: Match -> CT Equ
matchToEqu (Match vs lhs rhs) = do
  let lhsArgs = appArgs lhs
  ps <- mapM lhsArgToPat lhsArgs
  return $ Equ ps rhs

lhsArgToPat :: Exp -> CT Pat
lhsArgToPat e
  | (EVar v, []) <- appSplit e
  = return $ PVar v
  | (EConst (CCon c), xs) <- appSplit e
  = PCon <$> lookupCon c <*> mapM lhsArgToPat xs
  | otherwise
  = throwError $ "Illegal expression in pattern: " ++ show e

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
