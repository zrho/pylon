{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.Pylon.Core.Unify
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Dynamic higher order pattern unification for Pylon Core expressions.
--
-- The unifier takes a list of equations as input and tries to unify them by
-- applying rewrite rules to them until either no more rules apply or a
-- unification failure (that cannot possibly be fixed by a substitution) is
-- discovered. In the former case not all equations may have been solved, so
-- a list of the remaining (blocked) equations is returned alongside the
-- unifying substitution.
--------------------------------------------------------------------------------
module Language.Pylon.Core.Unify
  ( unify
  , UnifyResult
  , Equ
  , Context
  , USubst
  ) where
--------------------------------------------------------------------------------

import           Language.Pylon.Core.AST
import           Language.Pylon.Core.Util
import           Language.Pylon.Util
import           Language.Pylon.Util.Subst

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.State.Class   (MonadState, gets, modify)
import           Control.Monad.Reader.Class  (MonadReader, ask, asks, local)
import           Control.Monad.Error.Class   (MonadError, throwError, catchError)
import           Control.Concurrent.Supply

import           Data.Generics.Uniplate.Data (transformBi)
import           Data.Data                   (Data, Typeable)
import           Data.List                   (intersect)
import           Data.Monoid                 ((<>), mempty)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- API: Unification
--------------------------------------------------------------------------------

-- |
-- Tries to solve a list of equations by unification. Either reports back
-- a substitution as well a list of blocked equations or a permanent failure.
unify :: Supply -> [Equ] -> UnifyResult
unify s ps = either err id $ runRWSE (fromUnify unifyLoop) mempty state where
  state    = UnifyState ps [] s mempty
  err e    = error $ "Leaked unify error: " ++ e

-- |
-- Equation with two expressions to unify. Both expressions share a context.
data Equ = Equ Context Exp Exp deriving (Eq, Show, Typeable, Data)

-- |
-- Identifier to type map.
type Context = Map Ident Type

-- |
-- Substitution of holes in expressions.
type USubst  = Subst Hole Exp

-- |
-- Result of unification.
data UnifyResult
  = UnifyFail   Equ (Maybe UnifyError) -- ^ Equation not unifiable
  | UnifyResult [Equ] USubst           -- ^ Blocked equations and resulting substitution
  deriving (Show)

--------------------------------------------------------------------------------
-- Unify Monad
--------------------------------------------------------------------------------

newtype Unify a = Unify
  { fromUnify :: RWSE UnifyReader UnifyWriter UnifyState UnifyError a } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState  UnifyState
  , MonadError  UnifyError
  , MonadReader UnifyReader
  )

type UnifyWriter = ()
type UnifyReader = Context
type UnifyError  = String
data UnifyState  = UnifyState
  { usPending :: [Equ]
  , usBlocked :: [Equ]
  , usNames   :: Supply
  , usSubst   :: USubst
  } deriving (Show)

--------------------------------------------------------------------------------
-- Unification Loop
--------------------------------------------------------------------------------

-- |
-- Main loop of the unification algorithm: tries to unify pending equations with
-- the `unifyStep` function until no more equations are pending or unification
-- of an equation failed.
unifyLoop :: Unify UnifyResult
unifyLoop = nextPending >>= \mp -> case mp of
  Nothing       -> unifyResult
  Just equ      -> unifyStep equ >>= \st -> case st of
    SBlock      -> blockEqu equ >> unifyLoop
    SNext ps ss -> substEqus ss >> mapM_ pendingEqu ps >> unifyLoop
    SFail e     -> return $ UnifyFail equ e

-- |
-- Constructs a `UnifyResult` from the current unification state. Called by
-- `unifyLoop` when no more equations are pending.
--
-- Clears the twin variables in the resulting substitution to avoid leaking them
-- into non-unification code. Twin variables in the blocked equations must be
-- preserved for correct unification; this is not problematic, since expressions
-- in blocked equations should not be used in non-unification code anyway.
unifyResult :: Unify UnifyResult
unifyResult = UnifyResult
  <$> gets usBlocked
  <*> gets (fmap clearTwins . usSubst)

--------------------------------------------------------------------------------
-- ** Unification Steps
--------------------------------------------------------------------------------

-- |
-- Step in the unification loop
data Step
  = SNext  [Equ] USubst      -- ^ Progess: New equations and substitution to apply
  | SBlock                   -- ^ No progess: Block the equation.
  | SFail (Maybe UnifyError) -- ^ Failure: Equation can not be solved.
  deriving (Show)

unifyBlock :: Unify Step
unifyBlock = return SBlock

unifyFail :: Unify Step
unifyFail = return $ SFail Nothing

unifyNext :: [Equ] -> USubst -> Unify Step
unifyNext qs ss = return $ SNext qs ss

--------------------------------------------------------------------------------
-- Names and Identifiers
--------------------------------------------------------------------------------

type Pat = [Ident]

pattern ITwinL x = IGen "TwinL" x
pattern ITwinR x = IGen "TwinR" x
pattern IUnify x = IGen "Unify" x

freshInt :: Unify Int
freshInt = do
  (n, s) <- gets $ freshId . usNames
  modify $ \st -> st { usNames = s }
  return n

freshTwins :: Unify (Ident, Ident)
freshTwins = (ITwinL &&& ITwinR) <$> freshInt

freshIdent :: Unify Ident
freshIdent = IUnify <$> freshInt

freshHole :: Unify Hole
freshHole = freshInt

-- |
-- Checks whether the two identifiers are a pair of twins.
isTwinPair :: Ident -> Ident -> Bool
isTwinPair (ITwinL i) (ITwinR j) = i == j
isTwinPair (ITwinR i) (ITwinL j) = i == j
isTwinPair _          _          = False

-- |
-- Converts twin variables to ordinary unify variables.
clearTwins :: Exp -> Exp
clearTwins = transformBi go where
  go (ITwinL i) = IUnify i
  go (ITwinR i) = IUnify i
  go i          = i

--------------------------------------------------------------------------------
-- Problem Management
--------------------------------------------------------------------------------

-- |
-- Fetch the next pending equation, if there is any.
nextPending :: Unify (Maybe Equ)
nextPending = gets usPending >>= \mp -> case mp of
  []   -> return Nothing
  p:ps -> modify (\s -> s { usPending = ps }) >> return (Just p)

-- |
-- Insert an equation at the beginning of the pending list.
pendingEqu :: Equ -> Unify ()
pendingEqu e = modify $ \s -> s { usPending = e : usPending s }

-- |
-- Inserts an equation into the blocked list.
blockEqu :: Equ -> Unify ()
blockEqu e = modify $ \s -> s { usBlocked = e : usBlocked s }

-- |
-- Apply a substitution to all equations and log the substitution.
substEqus :: USubst -> Unify ()
substEqus ss = do
  p   <- gets usPending
  b   <- gets usBlocked
  ss' <- gets usSubst
  let p' = fmap (substEqu ss) (p <> b) -- todo: only unblock changed problems
  modify $ \s -> s { usPending = p', usBlocked = [], usSubst = ss <> ss' }

-- |
-- Applies a substitution to an equation.
substEqu :: USubst -> Equ -> Equ
substEqu ss (Equ c a b) = Equ (fmap (subst ss) c)(subst ss a) (subst ss b)

--------------------------------------------------------------------------------
-- Step: Dispatch
--------------------------------------------------------------------------------

-- |
-- Performs an unification rewrite step on an equation. Catches errors and
-- converts them to a fail step.
unifyStep :: Equ -> Unify Step
unifyStep (Equ c l r)      = catchError (local (const c) $ go l r) err where
  go x y | x == y          = unifyNext mempty mempty
  go (EFree x)  (EFree y)  = stepTwins x y
  go (ELam t x) (ELam s y) = stepLambda (t, x) (s, y)
  go (EPi  t x) (EPi  s y) = stepPi (t, x) (s, y)
  go (Flex a)   (Flex b)   = stepFF a b
  go (Flex a)   y          = stepFR a y
  go x          (Flex b)   = stepFR b x
  go x          y          = stepRR x y
  err                      = return . SFail . Just

-- |
-- Pattern that matches flex expressions: A hole applied to distinct free variables.
pattern Flex x <- (flex -> Just x)

-- |
-- Checks for flex expressions.
flex :: Exp -> Maybe (Hole, Pat)
flex (appSplit -> (EHole f, xs)) | all isFree xs, unique xs = Just (f, [ v | EFree v <- xs ])
flex _ = Nothing

--------------------------------------------------------------------------------
-- Step: Binders
--------------------------------------------------------------------------------

-- |
-- Step: Lambda abstraction. Opens the lambdas with a pair of fresh twin
-- variables, then tries to unify the types and the bodies of the abstractions.
--
-- Using twin variables allows unification progress, while the types of the
-- bound identifiers are not the same yet.
stepLambda :: (Type, Exp) -> (Type, Exp) -> Unify Step
stepLambda (t, x) (s, y) = do
  -- read context
  c <- ask
  -- twin variables for the arguments
  (i, j) <- freshTwins
  -- open the lamba abstractions
  let x' = open (EFree i) x
  let y' = open (EFree j) y
  -- create a context for the lambda body
  let c' = M.fromList [ (i, t), (j, s) ] <> c
  unifyNext [ Equ c t s, Equ c' x' y' ] mempty

-- |
-- Step: Pi abstraction. Opens the pis with a fresh variable, then tries to
-- unify the abstracted types with Universe (todo: check if this is correct)
-- and the respective bodies.
stepPi :: (Type, Type) -> (Type, Type) -> Unify Step
stepPi (t, x) (s, y) = do
  -- read context
  c <- ask
  -- open the pi abstraction
  i <- freshIdent
  let x' = open (EFree i) x
  let y' = open (EFree i) y
  -- create context for pi body
  let c' = M.insert i (EConst CUniv) c
  unifyNext [ Equ c t s, Equ c' x' y' ] mempty

--------------------------------------------------------------------------------
-- Step: Twin Variables
--------------------------------------------------------------------------------

-- |
-- Two variables that are not equal can be trivially unified if they are twins
-- and their types have converged.
--
-- todo: fail, if types cannt converge anymore
stepTwins :: Ident -> Ident -> Unify Step
stepTwins x y | isTwinPair x y = do
  s <- asks $ M.lookup x
  t <- asks $ M.lookup y
  case (s, t) of
    (Just s', Just t') | s' == t' -> unifyNext [] mempty
    _                             -> unifyBlock
stepTwins _ _ = unifyFail

--------------------------------------------------------------------------------
-- Step: Flex and Rigid combinations
--------------------------------------------------------------------------------

-- |
-- Rigid Rigid: When both application heads are equal and applied to the same
-- number of arguments, proceed by unifying the arguments position-wise.
stepRR :: Exp -> Exp -> Unify Step
stepRR (appSplit -> (f, xs)) (appSplit -> (g, ys))
  | f == g, length xs == length ys = ask >>= \c -> unifyNext (zipWith (Equ c) xs ys) mempty
  | otherwise                      = unifyFail

-- |
-- Flex Rigid
stepFR :: (Hole, Pat) -> Exp -> Unify Step
stepFR (f, xs) e@(appSplit -> (g, ys))
  | not (isVar g) || isVarIn g xs, f `S.notMember` freeHoles e = do
    hs <- mapM (\_ -> flip appPat xs <$> freshHole) ys
    c  <- ask
    let qs = zipWith (Equ c) ys hs
    ss <- mkSubst f <$> lamMult c xs (appMult g hs)
    unifyNext qs ss
stepFR _ _ = unifyBlock

-- |
-- Flex Flex: Check whether the holes are the same, then proceed accordingly.
stepFF :: (Hole, Pat) -> (Hole, Pat) -> Unify Step
stepFF (f, xs) (g, ys)
  | f == g    = stepFFSame f xs ys
  | otherwise = stepFFDiff (f, xs) (g, ys)

-- |
-- Flex Flex, same variable.
stepFFSame :: Hole -> Pat -> Pat -> Unify Step
stepFFSame f xs ys
  | length xs == length ys = do
    c <- ask
    g <- freshHole
    let zs = [ EFree x | (x, y) <- zip xs ys, x == y ]
    sf <- mkSubst f <$> lamMult c xs (appMult (EHole g) zs)
    unifyNext [] sf
  | otherwise = unifyBlock

-- |
-- Flex flex, different variable.
stepFFDiff :: (Hole, Pat) -> (Hole, Pat) -> Unify Step
stepFFDiff (f, xs) (g, ys) = do
  c <- ask
  -- create a fresh hole
  h <- freshHole
  -- apply h to the variables in the intersection of xs and ys
  let zs  = [ EFree z | z <- xs `intersect` ys ]
  let hzs = appMult (EHole h) zs
  -- substitute f and g by the application of h, abstracted over xs and ys
  sf <- mkSubst f <$> lamMult c xs hzs
  sg <- mkSubst g <$> lamMult c ys hzs
  unifyNext [] (sf <> sg)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

appPat :: Hole -> Pat -> Exp
appPat h xs = appMult (EHole h) [ EFree x | x <- xs ]

isVar :: Exp -> Bool
isVar (EFree _) = True
isVar _         = False

isVarIn :: Exp -> [Ident] -> Bool
isVarIn (EFree v) vs = v `elem` vs
isVarIn _         _  = False

lamMult :: Context -> [Ident] -> Exp -> Unify Exp
lamMult _ []     e = return e
lamMult c (i:is) e = case M.lookup i c of
  Just t  -> (ELam t . close i) <$> lamMult c is e
  Nothing -> throwError $ "Unknown variable " ++ show i
