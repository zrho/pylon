--------------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Language.Pylon.Core.Eval where
--------------------------------------------------------------------------------

import Prelude hiding (sequence, all)
import Language.Pylon.Util
import Language.Pylon.Util.Fold
import Language.Pylon.Util.Subst
import Language.Pylon.Core.AST
import Control.Monad (unless, when)
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans.State  (StateT, runStateT)
import Control.Monad.State.Class  (MonadState, gets, modify, get)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Reader       (Reader, runReader)
import Control.Monad.Error.Class  (MonadError, throwError)
import Data.Traversable (sequence, forM)
import Data.Foldable (forM_, fold, foldMap, all)
import Data.Monoid ((<>), mempty, mconcat)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

apply :: Exp -> [Exp] -> (Exp, [Type])
apply (EPi i t e) (a:as) = (second (t:)) (apply (subst (singletonSubst i a) e) as)
apply e               _     = (e, [])

--------------------------------------------------------------------------------
-- Head Normal Form
--------------------------------------------------------------------------------

nf :: Exp -> Exp
nf ee = spine ee [] where
  spine (EApp f x)   as     = spine f (x:as)
  spine (ELam i t e) []     = ELam i (nf t) (nf e)
  spine (ELam i _ e) (a:as) = spine (subst (singletonSubst i a) e) as
  spine e            as     = foldl EApp e $ fmap nf as

-- todo: implement
whnf :: Exp -> Exp
whnf = nf

--------------------------------------------------------------------------------
-- Alpha Equivalence
--------------------------------------------------------------------------------

alphaEq :: Exp -> Exp -> Bool
alphaEq x y = indexExp x == indexExp y

--------------------------------------------------------------------------------
-- De Bruijn Conversion
--------------------------------------------------------------------------------

newtype Indexing a = Indexing { fromIndexing :: Reader [Ident] a }
  deriving (Functor, Applicative, Monad, MonadReader [Ident])

indexExp :: Exp -> Exp
indexExp e = runReader (fromIndexing $ indexExp' e) []

indexExp' :: Exp -> Indexing Exp
indexExp' = cata alg where
  alg (FLam i t e) = ELam i <$> t <*> with [i] e
  alg (FPi i t e)  = EPi i <$> t <*> with [i] e
  alg (FLet bs e)  = with [ i | (i, _, _) <- bs ] $ do
    bs' <- forM bs $ \(i, t, x) -> (,,) i <$> t <*> x
    ELet bs' <$> e
  alg (FVar v) = asks $ EVar . fromMaybe v . fmap IIndex . indexOf v
  alg e = fmap inF $ sequence e
  with xs = local (xs ++)
