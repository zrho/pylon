{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Language.Pylon.STG.AST where

import Prelude       hiding (foldr)
import Data.Foldable (Foldable, foldr)


import           Data.Map (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- * Top Level
--------------------------------------------------------------------------------

type Program = [Bind]

--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

-- | Expressions.
data Exp
  = ELet     [Bind] Exp        -- ^ Local definition
  | ECase    Alts Default Exp  -- ^ Case expression
  | EApp     Var [Atom]        -- ^ Application
  | EForeign Var [Atom]        -- ^ Foreign function application
  | EPrim    PrimOp [Exp]      -- ^ Saturated build-in op
  | EAtom    Atom
  deriving (Eq, Show)

-- | Type
data Type = TInt | TAddr
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Atomic expressions.
data Atom
  = AVar Var
  | ALit Lit
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * Names and Bindings
--------------------------------------------------------------------------------

type Prog = [Bind]
data Bind = Bind Var Object deriving (Eq, Show)
type Var  = String
type Con  = Int

--------------------------------------------------------------------------------
-- * Primitives
--------------------------------------------------------------------------------

-- | Literal values.
data Lit
  = LInt Integer
  deriving (Eq, Show)

-- | Primitive type.
data Prim = PInt
  deriving (Eq, Show, Ord, Bounded, Enum)

-- | Primitive operations.
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
  | PForeign Var [Prim] Prim
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * Objects
--------------------------------------------------------------------------------

-- | Object
data Object
  = Fun [Var] Exp  -- ^ Function values
  | Pap Var [Atom] -- ^ Partial applications
  | Con Con [Atom] -- ^ Data constructor application
  | Thunk Exp      -- ^ Thunk
  | BlackHole      -- ^ Black hole
  | Error          -- ^ Error
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * Alternatives
--------------------------------------------------------------------------------

-- | List of alternatives.
data Alts
  = AAlts [AAlt]
  | PAlts [PAlt]
  deriving (Eq, Show)

-- | Algebraic alternative.
data AAlt = AAlt Con [Var] Exp
  deriving (Eq, Show)

-- | Primitive alternative.
data PAlt = PAlt Lit Exp
  deriving (Eq, Show)

-- | Default alternative.
data Default = Default (Maybe Var) Exp
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * Substitution and Free Vars
--------------------------------------------------------------------------------

bindVars :: [Bind] -> [Var]
bindVars = fmap $ \(Bind v _) -> v

without :: (Ord a, Foldable f) => Map a b -> f a -> Map a b
without m = foldr M.delete m
