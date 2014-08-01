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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash, ViewPatterns #-}
module Language.Pylon.Core.AST where
--------------------------------------------------------------------------------
import Language.Pylon.Util.Subst
import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Data
import Data.Maybe               (fromMaybe)
import Data.Map                 (Map)
import Data.String              (IsString, fromString)
import Data.Function            (on)
import Data.Foldable            (Foldable, fold)
import Data.Traversable         (Traversable)
import qualified Data.Set as S
--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

type Name = String

-- | Pylon Core program. Consists of expression bindings and data constructors.
data Program = Program
  { prData    :: Map String Con
  , prBind    :: Map String Bind
  } deriving (Eq, Show, Data, Typeable)

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
  } deriving (Eq, Show, Data, Typeable)

-- | Function binding: An expression and its type.
data Bind = Bind
  { bndBody :: Maybe [Match]
  , bndType :: Exp
  } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | Type and value level expressions.
data Exp
  = EConst Const
  | EApp   Exp Exp
  | EBind  Binder Exp
  | EPrim  PrimOp [Exp]
  | ELocal Index
  | EFree  Ident
  | EHole  Hole
  deriving (Eq, Show, Data, Typeable)

-- | Types are expressions.
type Type = Exp

instance Substitutable Ident Exp where
  subst s = transform f where
    f (EFree n) = fromMaybe (EFree n) $ substVar n s
    f x         = x

instance Substitutable Int Exp where
  subst s = transform f where
    f (EHole n) = fromMaybe (EHole n) $ substVar n s
    f x         = x

--------------------------------------------------------------------------------
-- Binder
--------------------------------------------------------------------------------

data Binder
  -- | lambda abstraction
  = BLam   { binderType :: Type }
  -- | dependent function
  | BPi    { binderType :: Type }
  -- | hole and guess binding
  | BGuess { binderType :: Type, binderGuess :: Maybe Exp }
  -- | let binders
  | BLet   { binderType :: Type, binderTerm :: Exp }
  deriving (Eq, Show, Data, Typeable)

pattern ELam a b     = EBind (BLam a) b
pattern EPi  a b     = EBind (BPi a) b
pattern EGuess a b c = EBind (BGuess a b) c
pattern ELet a b c   = EBind (BLet a b) c

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

data Match = Match [(Ident, Exp)] Exp Exp
  deriving (Eq, Show, Data, Typeable)

-- | Constant value.
data Const
  = CLit      Lit
  | CCon      Name
  | CPrim     Prim
  | CPrimUniv
  | CUniv
  deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------------
-- Primitives and Literals
--------------------------------------------------------------------------------

-- | Literal value.
data Lit = LInt Integer deriving (Eq, Show, Data, Typeable)

-- | Primitive value.
data Prim = PInt deriving (Eq, Show, Ord, Data, Typeable)

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
  deriving (Eq, Show, Ord, Data, Typeable)

--------------------------------------------------------------------------------
-- Identifier
--------------------------------------------------------------------------------

type Hole  = Int
type Index = Int

data Ident
  = ISource String
  | IGen    String Int
  deriving (Eq, Ord, Data, Typeable)

instance Show Ident where
  show (ISource n)   = n
  show (IGen t i)    = concat ["{gen ", show t, ":", show i ,"}"]

instance IsString Ident where
  fromString = ISource

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

matchArity :: Match -> Int
matchArity (Match _ lhs _) = length $ appArgs lhs

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

appArgs :: Exp -> [Exp]
appArgs (EApp f x) = appArgs f ++ [x]
appArgs e          = []

appFun :: Exp -> Exp
appFun (EApp f _) = appFun f
appFun e          = e

appSplit :: Exp -> (Exp, [Exp])
appSplit e = (appFun e, appArgs e)

appMult :: Exp -> [Exp] -> Exp
appMult f xs = foldl EApp f xs

isFree :: Exp -> Bool
isFree (EFree _) = True
isFree _         = False

isConst :: Exp -> Bool
isConst (EConst _) = True
isConst _          = False
