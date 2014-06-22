--------------------------------------------------------------------------------
-- |
-- Module      : Pylon.Core.AST
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Pylon Core language: Minimal dependently typed language with algebraic
-- data types.
--------------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE MagicHash #-}
module Language.Pylon.Core.AST where
--------------------------------------------------------------------------------

import Language.Pylon.Util.Fold
import Language.Pylon.Util.Subst
import Data.Maybe               (fromMaybe)
import Data.Map                 (Map)
import Data.String              (IsString, fromString)
import Data.Function            (on)
import Data.Foldable            (Foldable)
import Data.Traversable         (Traversable)

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

type Name = String

-- | Pylon Core program. Consists of expression bindings and data constructors.
data Program = Program
  { prData    :: Map String Con
  , prBind    :: Map String Bind
  } deriving (Eq, Show)

-- | Data constructor.
-- |
-- | Each constructor has an index which must be unique among all constructors
-- | of the same data type.
-- |
-- | The type of the constructor must be delared as a function of universe type
-- | in the binding section of the program.
data Con = Con
  { conIndex :: Int
  , conType  :: Exp
  } deriving (Eq, Show)

-- | Function binding: An expression and its type.
data Bind = Bind
  { bndExp :: Exp
  , bndType :: Exp
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | Type and value level expressions.
data ExpF e
  = FConst Const
  | FApp   e e
  | FLam   (Ident, e)  e
  | FPi    (Ident, e)  e
  | FLet   [(Ident, e, e)] e
  | FCase  (Alts e) (Ident, e) e
  | FVar   Ident
  | FPrim  PrimOp [e]
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Exp = Exp { fromExp :: ExpF Exp }

instance Fixpoint ExpF Exp where
  inF  = Exp
  outF = fromExp

-- todo: prettier
instance Show Exp where
  show (EVar i)        = show i
  show (EConst c)      = show c
  show (EApp f x)      = concat ["(", show f, " ", show x, ")"]
  show (ELam (i, t) e) = concat ["\\(", show i, ": ", show t, ") -> ", show e]
  show (EPi (i, t) e)  = concat ["(", show i, ": ", show t, ") -> ", show e]
  show e               = "(" ++ show (fromExp e) ++ ")"

instance Eq Exp where
  (==) = (==) `on` fromExp

instance Substitutable Ident Exp where
  subst s = cata alg where
    alg (FVar j) = fromMaybe (EVar j) $ substVar j s
    alg f        = inF f

pattern EConst a    = Exp (FConst a)
pattern EApp a b    = Exp (FApp a b)
pattern ELam a b    = Exp (FLam a b)
pattern EPi a b     = Exp (FPi a b)
pattern ELet a b    = Exp (FLet a b)
pattern ECase a b c = Exp (FCase a b c)
pattern EVar a      = Exp (FVar a)
pattern EPrim a b   = Exp (FPrim a b)

-- | Types are expressions.
type Type = Exp

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

data Alts e
  = AAlts [AAlt e]
  | PAlts [PAlt e]
  deriving (Eq, Show, Functor, Foldable, Traversable)

data AAlt e = AAlt Name [Ident] e
  deriving (Eq, Show, Functor, Foldable, Traversable)

data PAlt e = PAlt Lit e
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Constant value.
data Const
  = CLit      Lit
  | CCon      Name
  | CGlobal   Name
  | CPrim     Prim
  | CPrimUniv
  | CUniv
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Primitives and Literals
--------------------------------------------------------------------------------

-- | Literal value.
data Lit = LInt Integer deriving (Eq, Show)

-- | Primitive value.
data Prim = PInt deriving (Eq, Show, Ord)

-- | Primitive operation.
data PrimOp
  = PPlus Prim
  | PMult Prim
  | PDiv Prim
  | PMinus Prim
  | PEq Prim
  | PLt Prim
  | PLte Prim
  | PGt Prim
  | PGte Prim
  | PForeign Name [Prim] Prim
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Identifier
--------------------------------------------------------------------------------

-- | Identifier. May contain a unique index.
data Ident = Ident
  { iName   :: String
  , iUnique :: Int
  }

instance Show Ident where
  show (Ident n u) = n ++ "{" ++ show u ++ "}"

instance Eq Ident where
  (==) = (==) `on` iUnique

instance Ord Ident where
  compare = compare `on` iUnique

instance IsString Ident where
  fromString s = Ident s (-1)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Arity of a constructor.
conArity :: Con -> Int
conArity = typeArity . conType

-- | Arity of a type.
typeArity :: Exp -> Int
typeArity (EPi _ e) = 1 + typeArity e
typeArity _         = 0

funResult :: Type -> Type
funResult (EPi _ e) = funResult e
funResult e         = e

funArgs :: Type -> [Type]
funArgs (EPi (_, t) e) = funArgs e ++ [t]
funArgs e              = []