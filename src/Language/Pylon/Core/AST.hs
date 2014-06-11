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
module Language.Pylon.Core.AST where
--------------------------------------------------------------------------------

import Data.Map (Map)

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

type Name = String

-- | Pylon Core program. Consists of expression bindings and data constructors.
data Program = Program
  { prData :: Map Name Con
  , prBind :: Map Name Bind
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
-- Expression Level
--------------------------------------------------------------------------------

-- | Type and value level expressions.
data Exp
  = EConst Const
  | EApp   Exp   Exp
  | ELam   Type  Exp
  | EPi    Type  Exp
  | ELet   [(Exp, Type)] Exp
  | ECase  [(Pat, Exp)] Exp Exp
  | ELocal Int
  deriving (Eq, Show)

-- | Types are expressions.
type Type = Exp

-- | Pattern for case expressions. May only be algebraic.
data Pat = PCon Name Int deriving (Eq, Show)

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
-- Helper Functions
--------------------------------------------------------------------------------

-- | Arity of a constructor.
conArity :: Con -> Int
conArity = typeArity . conType

-- | Arity of a type.
typeArity :: Exp -> Int
typeArity (EPi _ e) = 1 + typeArity e
typeArity _         = 0