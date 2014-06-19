--------------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Language.Pylon.Core.Eval where
--------------------------------------------------------------------------------

import Prelude hiding (sequence, all)
import Language.Pylon.Util.Fold
import Language.Pylon.Util.Subst
import Language.Pylon.Core.AST
import Control.Monad (unless, when)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.State.Class (MonadState, gets, modify, get)
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Traversable (sequence, forM)
import Data.Foldable (forM_, fold, foldMap, all)
import Data.Monoid ((<>), mempty, mconcat)
import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

apply :: Exp -> [Exp] -> (Exp, [Type])
apply (EPi (i, t) e) (a:as) = (second (t:)) (apply (subst (singletonSubst i a) e) as)
apply e               _     = (e, [])

--------------------------------------------------------------------------------
-- Head Normal Form
--------------------------------------------------------------------------------

nf :: Exp -> Exp
nf ee = spine ee [] where
  spine (EApp f x)      as     = spine f (x:as)
  spine (ELam (i, t) e) []     = ELam (i, nf t) (nf e)
  spine (ELam (i, _) e) (a:as) = spine (subst (singletonSubst i a) e) as
  spine e               as     = foldl EApp e $ fmap nf as

-- todo: implement
whnf :: Exp -> Exp
whnf = nf

--------------------------------------------------------------------------------
-- Alpha Equivalence
--------------------------------------------------------------------------------

alphaEq :: Exp -> Exp -> Either String (Subst Ident Exp)
alphaEq x y = do
  u <-  runUnify (unify x y)
  let bs = bound x <> bound y
  forM_ (M.keys $ fromSubst u) $ \i -> unless (S.member i bs)
    $ throwError $ "Variable escaped: " ++ show i
  return u

bound :: Exp -> S.Set Ident
bound = cata alg where
  alg f@(FLam (i, _) _)     = S.insert i $ fold f
  alg f@(FPi (i, _) _)      = S.insert i $ fold f
  alg f@(FLet bs _)         = (S.fromList $ fmap (\(i, _, _) -> i) bs) <> fold f
  alg f@(FCase as (d, _) _) = S.singleton d <> fold f <> case as of
    AAlts as -> S.fromList $ foldMap (\(AAlt _ vs _) -> vs) as
    _        -> S.empty
  alg f                     = fold f

--------------------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------------------

newtype Unify a = Unify { fromUnify :: StateT UnifyState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState UnifyState, MonadError String)

data UnifyState = UnifyState
  { usSubst :: Subst Ident Exp
  , usBound :: S.Set Ident
  }

runUnify :: Unify a -> Either String (Subst Ident Exp)
runUnify go = fmap (usSubst . snd) $ runStateT (fromUnify go) $ UnifyState mempty mempty

unify :: Exp -> Exp -> Unify ()
unify el er = gets usSubst >>= \s -> go (subst s el) (subst s er) where
  -- variables
  go (EVar i) (EVar j) | i == j = return ()
  go (EVar i) e = do
    bs <- gets usBound
    when (occurs i e) $ throwError $ "Occurs check failure: " ++ show (i, e)
    when (S.member i bs) $ throwError $ "Failed to unify bound variable: " ++ show (i, e)
    tellSubst i e
  go e (EVar j)  = go (EVar j) e
  -- other expressions
  go (EConst c) (EConst d) | c == d  = return ()
  go (EApp f x) (EApp g y)           = unify f g >> unify x y
  go (ELam (i, t) x) (ELam (j, l) y) = do
    tellBound i j
    unify t l >> unify x y
  go (EPi (i, t) x) (EPi (j, l) y) = do
    tellBound i j
    unify t l >> unify x y
  go (ELet bs x) (ELet cs y) = do
    unless (length bs == length cs) $ throwError "Mismatch in let binding count."
    forM_ (zip bs cs) $ \((i, t, a), (j, l, b)) -> do
      tellBound i j
      unify t l
    unify x y
  go (ECase ps (ci, ce) x) (ECase qs (di, de) y) = do
    tellBound ci di
    unify x y
    unify ce de
    goAlts ps qs
  go x y = throwError $ "Unify mismatch in: " ++ show (x, y)
  goAlts (AAlts as) (AAlts bs) = forM_ (zip as bs) $
    \((AAlt c vs a), (AAlt d ws b)) -> do
      unless (c == d) $ throwError "Constructor mismatch in case expression."
      forM_ (zip vs ws) $ \(v, w) -> tellBound v w
      unify a b
  goAlts (PAlts as) (PAlts bs) = forM_ (zip as bs) $
    \((PAlt x a), (PAlt y b)) -> do
      unless (x == y) $ throwError "Literal mismatch in case expression."
      unify a b

  {-
$ 
  -}

occurs :: Ident -> Exp -> Bool
occurs _ _ = False -- todo

tellSubst :: Ident -> Exp -> Unify ()
tellSubst i e = do
  ss <- gets usSubst
  modify $ \s -> s { usSubst = (Subst $ M.singleton i e) <> ss }

tellBound :: Ident -> Ident -> Unify ()
tellBound i j = do
  bs <- gets usBound
  modify $ \s -> s { usBound = S.insert j bs }
  tellSubst i $ EVar j