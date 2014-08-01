{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Language.Pylon.Core.Unify
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--------------------------------------------------------------------------------
module Language.Pylon.Core.Unify
  ( unify
  , Equ (..)
  , UnifyResult (..)
  , USubst
  , Context
  ) where
--------------------------------------------------------------------------------
import Language.Pylon.Core.AST
import Language.Pylon.Core.Util
import Language.Pylon.Util
import Language.Pylon.Util.Subst
import Language.Pylon.Util.Name
import Control.Applicative
import Control.Arrow
import Control.Monad (guard, foldM, forM_)
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState, get, gets, modify)
import Control.Monad.Error.Class (MonadError, Error, throwError, catchError, strMsg)
import Control.Concurrent.Supply
import Data.Generics.Uniplate.Data
import Data.Data
import Data.Typeable
import Data.List (intersect)
import Data.Monoid
import Data.Foldable (fold)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- * Unification
--------------------------------------------------------------------------------

-- | Equation with two expressions to unify. Both expressions share the
--   given context.
data Equ = Equ Context Exp Exp deriving (Eq, Show, Typeable, Data)

-- | Identifier to type map.
type Context = Map Ident Type

-- | Substitution of holes in expressions.
type USubst  = Subst Hole Exp

-- | Result of unification.
data UnifyResult
  -- | Equation not unifiable.
  = UnifyFail Equ
  -- | Blocked equations and resulting substitution.
  | UnifyResult [Equ] USubst
  deriving (Show)

-- | Tries to unify the given equations.
unify :: (Error e, MonadError e m, MonadSupply m) => [Equ] -> m UnifyResult
unify ps = do
  su <- supplySplit
  liftEitherStr $ evalStateT (fromUnify unifyLoop) $ UnifyState ps [] mempty su

--------------------------------------------------------------------------------
-- ** Unify Loop
--------------------------------------------------------------------------------

newtype Unify a = Unify { fromUnify :: StateT UnifyState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState UnifyState, MonadError String)

data UnifyState = UnifyState
  { usPending :: [Equ]
  , usBlocked :: [Equ]
  , usSubst   :: USubst
  , usNames   :: Supply
  } deriving (Show)

type Pat = [Ident]

unifyLoop :: Unify UnifyResult
unifyLoop = nextPending >>= \mp -> case mp of
  Nothing          -> unifyStop
  Just (Equ c x y) -> unifyStep c x y >>= \st -> case st of
    SBlock         -> unifyLoop
    SFail          -> return $ UnifyFail $ Equ c x y
    SNext ps ss    -> unifySubst ss >> mapM_ pendingEqu ps >> unifyLoop

unifyStop :: Unify UnifyResult
unifyStop = UnifyResult
  <$> gets usBlocked
  <*> gets (fmap clearTwins . usSubst)

--------------------------------------------------------------------------------
-- ** Unification Steps
--------------------------------------------------------------------------------

data Step
  = SNext  [Equ] USubst
  | SBlock
  | SFail
  deriving (Show)

unifyBlock :: Unify Step
unifyBlock = return SBlock

unifyFail :: Unify Step
unifyFail = return SFail

unifyNext :: [Equ] -> USubst -> Unify Step
unifyNext qs ss = return $ SNext qs ss

--------------------------------------------------------------------------------
-- ** Names and Identifiers
--------------------------------------------------------------------------------

pattern ITwinL x = IGen "TwinL" x
pattern ITwinR x = IGen "TwinR" x
pattern IUnify x = IGen "Unify" x
pattern ETwinL x = EFree (ITwinL x)
pattern ETwinR x = EFree (ITwinR x)

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

--------------------------------------------------------------------------------
-- ** Problem Management
--------------------------------------------------------------------------------

nextPending :: Unify (Maybe Equ)
nextPending = gets usPending >>= \mp -> case mp of
  []   -> return Nothing
  p:ps -> modify (\s -> s { usPending = ps }) >> return (Just p)

-- | Inserts an equation at the beginning of the pending list.
pendingEqu :: Equ -> Unify ()
pendingEqu e = modify $ \s -> s { usPending = e : usPending s }

-- | Inserts an equation into the blocked list.
blockEqu :: Equ -> Unify ()
blockEqu e = modify $ \s -> s { usBlocked = e : usBlocked s }

unifySubst :: USubst -> Unify ()
unifySubst ss = do
  p <- gets usPending
  b <- gets usBlocked
  r <- gets usSubst
  let p' = fmap (substEqu ss) (p <> b)
  -- todo: only unblock changed problems
  modify $ \s -> s { usPending = p', usBlocked = [], usSubst = r <> ss }

-- | Applies a substitution to an equation.
substEqu :: USubst -> Equ -> Equ
substEqu ss (Equ c a b) = Equ (fmap (subst ss) c)(subst ss a) (subst ss b)

--------------------------------------------------------------------------------
-- * Transformation step
--------------------------------------------------------------------------------

unifyStep :: Context -> Exp -> Exp -> Unify Step
unifyStep _ x y | x == y          = unifyNext mempty mempty
unifyStep c (EFree x)  (EFree y)  = stepTwins c x y
unifyStep c (ELam t x) (ELam s y) = stepLambda c (t, x) (s, y)
unifyStep c (EPi  t x) (EPi  s y) = stepPi c (t, x) (s, y)
unifyStep c (Flex a)   (Flex b)   = stepFF c a b
unifyStep c (Flex a)   y          = stepFR c a y
unifyStep c x          (Flex b)   = stepFR c b x
unifyStep c x          y          = stepRR c x y

pattern Flex x <- (flex -> Just x)

flex :: Exp -> Maybe (Hole, Pat)
flex (appSplit -> (EHole f, xs)) | all isFree xs, unique xs = Just (f, [ v | EFree v <- xs ])
flex _ = Nothing

--------------------------------------------------------------------------------
-- ** Binder and Variable transformation
--------------------------------------------------------------------------------

stepLambda :: Context -> (Type, Exp) -> (Type, Exp) -> Unify Step
stepLambda c (t, x) (s, y) = do
  -- twin variables for the arguments
  (i, j) <- freshTwins
  -- t must be a pi type
  ta <- EHole <$> freshHole
  tr <- EHole <$> freshHole
  let tq = Equ c (EPi ta tr) t
  -- s must be a pi type
  sa <- EHole <$> freshHole
  sr <- EHole <$> freshHole
  let sq = Equ c (EPi sa sr) s
  -- open the lamba abstractions
  let x' = open (EFree i) x
  let y' = open (EFree j) y
  -- create a context for the lambda body
  let c' = M.fromList [ (i, ta), (j, sa) ] <> c
  unifyNext [ Equ c t s, tq, sq, Equ c' x' y' ] mempty

stepPi :: Context -> (Type, Type) -> (Type, Type) -> Unify Step
stepPi c (t, x) (s, y) = do
  -- open the pi abstraction
  i <- freshIdent
  let x' = open (EFree i) x
  let y' = open (EFree i) y
  -- create context for pi body
  let c' = M.insert i (EConst CUniv) c
  unifyNext [ Equ c t s, Equ c' x' y' ] mempty

--------------------------------------------------------------------------------
-- ** Twin Variables
--------------------------------------------------------------------------------

-- | Two variables that are not equal can be trivially unified if they are
-- | twins and their types have converged.
stepTwins :: Context -> Ident -> Ident -> Unify Step
stepTwins c x y
  | isTwinPair x y
  , Just t <- M.lookup x c
  , Just s <- M.lookup y c
  , s == t                   = unifyNext [] mempty
  | otherwise                = unifyBlock

-- | Checks whether the two identifiers are a pair of twins.
isTwinPair :: Ident -> Ident -> Bool
isTwinPair (ITwinL i) (ITwinR j) = i == j
isTwinPair (ITwinR i) (ITwinL j) = i == j
isTwinPair _          _          = False

-- | Converts twin variables to ordinary unify variables.
clearTwins :: Exp -> Exp
clearTwins = transformBi go where
  go (ITwinL i) = IUnify i
  go (ITwinR i) = IUnify i
  go i          = i

--------------------------------------------------------------------------------
-- ** Flex/Rigid transformation
--------------------------------------------------------------------------------

-- | Rigid Rigid.
stepRR :: Context -> Exp -> Exp -> Unify Step
stepRR c (appSplit -> (f, xs)) (appSplit -> (g, ys))
  | f == g    = unifyNext (zipWith (Equ c) xs ys) mempty
  | otherwise = unifyFail

-- | Flex Rigid.
stepFR :: Context -> (Hole, Pat) -> Exp -> Unify Step
stepFR c (f, xs) e@(appSplit -> (g, ys))
  | not (isVar g) || isVarIn g xs, f `S.notMember` freeHoles e = do
    hs <- mapM (\_ -> flip appPat xs <$> freshHole) ys
    let qs = zipWith (Equ c) ys hs
    ss <- mkSubst f <$> lamMult c xs (appMult g hs)
    unifyNext qs ss
stepFR _ _ _ = unifyBlock

-- | Flex Flex.
stepFF :: Context -> (Hole, Pat) -> (Hole, Pat) -> Unify Step
stepFF c (f, xs) (g, ys)
  | f == g    = stepFFSame c f xs ys
  | otherwise = stepFFDiff c (f, xs) (g, ys)

-- | Flex Flex, same variable.
stepFFSame :: Context -> Hole -> Pat -> Pat -> Unify Step
stepFFSame c f xs ys
  | length xs == length ys = do
    g <- freshHole
    let zs = [ EFree x | (x, y) <- zip xs ys, x == y ]
    sf <- mkSubst f <$> lamMult c xs (appMult (EHole g) zs)
    unifyNext [] sf
  | otherwise = unifyBlock

-- | Flex flex, different variable.
stepFFDiff :: Context -> (Hole, Pat) -> (Hole, Pat) -> Unify Step
stepFFDiff c (f, xs) (g, ys) = do
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
-- * Utilities
--------------------------------------------------------------------------------

appPat :: Hole -> Pat -> Exp
appPat h xs = appMult (EHole h) [ EFree x | x <- xs ]

isVar :: Exp -> Bool
isVar (EFree v) = True
isVar _         = False

isVarIn :: Exp -> [Ident] -> Bool
isVarIn (EFree v) vs = v `elem` vs
isVarIn _         _  = False

lamMult :: Context -> [Ident] -> Exp -> Unify Exp
lamMult c []     e = return e
lamMult c (i:is) e = case M.lookup i c of
  Just t  -> (ELam t . close i) <$> lamMult c is e
  Nothing -> throwError $ "Unknown variable " ++ show i
