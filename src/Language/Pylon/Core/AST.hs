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
  | EVar   Ident
  | EPrim  PrimOp [Exp]
  deriving (Eq, Data, Typeable)

-- todo: prettier
instance Show Exp where
  show (EVar i)     = show i
  show (EConst c)   = show c
  show (EApp f x)   = concat ["(", show f, " ", show x, ")"]
  show (ELam i t e) = concat ["\\(", show i, ": ", show t, ") -> ", show e]
  show (EPi i t e)  = concat ["(", show i, ": ", show t, ") -> ", show e]

instance Substitutable Ident Exp where
  subst s = transform f where
    f (EVar j) = fromMaybe (EVar j) $ substVar j s
    f x        = x

-- | Types are expressions.
type Type = Exp

--------------------------------------------------------------------------------
-- Binder
--------------------------------------------------------------------------------

data Binder
  -- | lambda abstraction
  = BLam  { binderIdent :: Ident, binderType :: Type }
  -- | dependent function
  | BPi   { binderIdent :: Ident, binderType :: Type }
  -- | hole and guess binding
  | BHole { binderIdent :: Ident, binderType :: Type, binderGuess :: Maybe Exp }
  -- | let binders
  | BLet  { binderIdent :: Ident, binderType :: Type, binderTerm :: Exp }
  deriving (Eq, Show, Data, Typeable)

pattern ELam a b c    = EBind (BLam a b) c
pattern EPi  a b c    = EBind (BPi a b) c
pattern EHole a b c d = EBind (BHole a b c) d
pattern ELet a b c d  = EBind (BLet a b c) d

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

data Match = Match [(Ident, Exp)] Exp Exp
  deriving (Eq, Show, Data, Typeable)

-- | Constant value.
data Const
  = CLit      Lit
  | CCon      Name
  | CGlobal   Name
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

data Ident
  = ISource String
  | IScoped String Int
  | IGen    String Int
  | IIndex  Int
  deriving (Eq, Ord, Data, Typeable)

instance Show Ident where
  show (ISource n)   = n
  show (IScoped n s) = concat ["{scp ", n, ":", show s, "}"]
  show (IGen t i)    = concat ["{gen ", show t, ":", show i ,"}"]
  show (IIndex i)    = concat ["{idx ", show i ,"}"]

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
typeArity (EPi _ _ e) = 1 + typeArity e
typeArity _         = 0

funResult :: Type -> Type
funResult (EPi _ _ e) = funResult e
funResult e         = e

funArgs :: Type -> [(Ident, Type)]
funArgs (EPi i t e) = funArgs e ++ [(i, t)]
funArgs e              = []

appArgs :: Exp -> [Exp]
appArgs (EApp f x) = appArgs f ++ [x]
appArgs e          = []

appFun :: Exp -> Exp
appFun (EApp f _) = appFun f
appFun e          = e

appSplit :: Exp -> (Exp, [Exp])
appSplit e = (appFun e, appArgs e)
