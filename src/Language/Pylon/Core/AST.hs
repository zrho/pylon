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
module Language.Pylon.Core.AST where
--------------------------------------------------------------------------------

import Language.Pylon.Util.Fold
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
  { prData :: Map String Con
  , prBind :: Map String Bind
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
  { bndExp  :: Exp 
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
  | FCase  [(Pat, e)] (Ident, e) e
  | FVar   Ident
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Exp = Exp { fromExp :: ExpF Exp }

instance Fixpoint ExpF Exp where
  inF  = Exp
  outF = fromExp

instance Show Exp where
  show = show . fromExp

instance Eq Exp where
  (==) = (==) `on` fromExp

pattern EConst a    = Exp (FConst a)
pattern EApp a b    = Exp (FApp a b)
pattern ELam a b    = Exp (FLam a b)
pattern EPi a b     = Exp (FPi a b)
pattern ELet a b    = Exp (FLet a b)
pattern ECase a b c = Exp (FCase a b c)
pattern EVar a      = Exp (FVar a)

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- | Types are expressions.
type Type = Exp

-- | Pattern for case expressions. May only be algebraic.
data Pat = PCon Name [Ident] deriving (Eq, Show)

-- | Constant value.
data Const
  = CLit    Lit
  | CCon    Name
  | CGlobal Name
  | CUniv
  deriving (Eq, Show)

-- | Literal value.
data Lit = LInt Integer deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Identifier
--------------------------------------------------------------------------------

-- | Identifier. May contain a unique index.
data Ident = Ident
  { iName   :: String
  , iUnique :: Int
  } deriving (Show)

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