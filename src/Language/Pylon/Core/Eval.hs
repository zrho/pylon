--------------------------------------------------------------------------------
module Language.Pylon.Core.Eval where
--------------------------------------------------------------------------------

import Language.Pylon.Core.AST

--------------------------------------------------------------------------------
-- Head Normal Form
--------------------------------------------------------------------------------

apply :: Exp -> Exp -> Exp
apply f x = go 0 f where
  go i (ELocal j)        = if i == j then x else ELocal j
  go i (EConst c)        = EConst c
  go i (EApp g y)        = EApp (go i g) $ go i y
  go i (ELam t e)        = ELam (go i t) $ go (i + 1) e
  go i (ELet es e)       = ELet (fmap (goLet j) es) (go j e) where j = i + length es
  go i (ECase bs d e)    = ECase (fmap (goBind i) bs) (go (i + 1) d) (go i e)
  goLet i (e, t)         = (go i e, go i t)
  goBind i (PCon c n, e) = (PCon c n, go (i + n) e)

nf :: Exp -> Exp
nf ee = spine ee [] where
  spine (EApp f x) as     = spine f (x:as)
  spine (ELam t e) []     = ELam (nf t) (nf e)
  spine (ELam _ e) (a:as) = spine (apply e a) as
  spine e          as     = app e as
  app   e          as     = foldl EApp e $ fmap nf as

betaEq :: Exp -> Exp -> Bool
betaEq x y = nf x == nf y