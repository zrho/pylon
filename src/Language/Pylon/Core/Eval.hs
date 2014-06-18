--------------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}
module Language.Pylon.Core.Eval where
--------------------------------------------------------------------------------

import Language.Pylon.Core.AST
import Control.Arrow

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

subst :: Ident -> Exp -> Exp -> Exp
subst i s ee = case ee of
  EVar j            -> if i == j then s else EVar j
  EConst c          -> EConst c
  EApp g y          -> EApp (r g) (r y)
  ELam (j, t) e     -> ELam (j, r t) (r e)
  EPi (j, t) e      -> EPi (j, r t) (r e)
  ELet es e         -> ELet (fmap (\(j, x, t) -> (j, r x, r t)) es) (r e)
  ECase bs (j, d) e -> ECase (fmap (second r) bs) (j, r d) (r e)
  where r = subst i s

--------------------------------------------------------------------------------
-- Head Normal Form
--------------------------------------------------------------------------------

nf :: Exp -> Exp
nf ee = spine ee [] where
  spine (EApp f x)      as     = spine f (x:as)
  spine (ELam (i, t) e) []     = ELam (i, nf t) (nf e)
  spine (ELam (i, _) e) (a:as) = spine (subst i a e) as
  spine e               as     = foldl EApp e $ fmap nf as

-- todo: implement
whnf :: Exp -> Exp
whnf = nf

--------------------------------------------------------------------------------
-- Alpha Equivalence
--------------------------------------------------------------------------------

alphaEq :: Exp -> Exp -> Bool
alphaEq x y = error "todo"