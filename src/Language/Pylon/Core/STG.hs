--------------------------------------------------------------------------------
-- |
-- Module      : Pylon.Core.STG
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Translation of Pylon Core to STG.
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Pylon.Core.STG where
--------------------------------------------------------------------------------

import           Language.Pylon.Util
import           Language.Pylon.Util.Name
import           Language.Pylon.Core.AST
import qualified Language.Pylon.Core.CaseTree as CT
import qualified Language.Pylon.STG.AST       as STG

import Control.Monad              (replicateM, forM)
import Control.Monad.Error.Class  (MonadError, throwError)
import Control.Monad.State.Class  (MonadState, modify, gets)
import Control.Monad.Trans.State  (StateT, runStateT)
import Control.Monad.Trans.Class  (lift)
import Control.Applicative hiding (Const)

import           Data.Foldable (toList, fold)
import qualified Data.Map as M
import           Data.Monoid

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

newtype Trans a = Trans { fromTrans :: StateT TransState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState TransState, MonadError String)

data TransState = TransState
  { tsProgram :: Program
  , tsNames   :: Int
  } deriving (Eq, Show)

instance MonadName [Char] Trans where
  freshName = do
    n <- gets tsNames
    modify $ \s -> s { tsNames = n + 1 }
    return $ "_g" ++ show n

runTrans :: Trans a -> Program -> Either String a
runTrans go p = fmap fst $ runStateT (fromTrans go) $ TransState p 0

-- | Looks up a constructor in the program.
lookupCon :: Name -> Trans Con
lookupCon name = do
  dat <- gets $ prData . tsProgram
  case M.lookup name dat of
    Just c  -> return c
    Nothing -> throwError $ "No such constructor: " ++ name

--------------------------------------------------------------------------------
-- Programs
--------------------------------------------------------------------------------

genProgram :: Trans STG.Program
genProgram = do
  p  <- gets tsProgram
  mapM genBind $ M.toList $ prBind p

-- | STG code for an expression binding.
-- |
-- | Creates a function object for lambda expressions and a thunk otherwise.
genBind :: (Name, Bind) -> Trans STG.Bind
genBind (name, Bind Nothing t)   = error "todo STG imports"
genBind (name, Bind (Just ms) _) = STG.Bind name <$> genMatches ms

--------------------------------------------------------------------------------
-- Matches
--------------------------------------------------------------------------------

genMatches :: [Match] -> Trans STG.Object
genMatches [] = throwError "No matches."
genMatches ms = do
  qs <- mapM genMatchEqu ms
  let n   = matchArity $ head ms
  let def = EConst $ CGlobal "error" --todo
  let (vs, ct) = CT.toCaseTree n qs def
  e <- genCaseTree ct
  return $ if n == 0
    then STG.Thunk e
    else STG.Fun (fmap toName vs) e

genMatchEqu :: Match -> Trans CT.Equ
genMatchEqu (Match vs lhs rhs) = do
  let lhsArgs = appArgs lhs
  ps <- mapM genLhsArgPat lhsArgs
  return $ CT.Equ ps rhs

genLhsArgPat :: Exp -> Trans CT.Pat
genLhsArgPat e 
  | (EVar v, []) <- appSplit e
  = return $ CT.PVar v
  | (EConst (CCon c), xs) <- appSplit e
  = CT.PCon <$> lookupCon c <*> mapM genLhsArgPat xs
  | otherwise
  = throwError $ "Illegal expression in pattern: " ++ show e


genCaseTree :: CT.CaseTree -> Trans STG.Exp
genCaseTree (CT.CExp e) = genExp e
genCaseTree (CT.CCase v cls) = do
  alts <- mapM genClause cls
  let def  = STG.Default Nothing $ STG.EAtom $ STG.AVar "stg.error"
  let scr  = toVar $ toName v
  return $ STG.ECase (STG.AAlts alts) def scr

genClause :: CT.Clause -> Trans STG.AAlt
genClause (CT.Clause c vs e) = do
  e' <- genCaseTree e
  let vs' = fmap toName vs
  return $ STG.AAlt (conIndex c) vs' e'

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | STG code for Pylon Core expressions.
-- |
-- | Since no type level code is translated to STG, pi expressions have no
-- | STG translation.
genExp :: Exp -> Trans STG.Exp
genExp (EConst c )     = genConst c
genExp (EPi _ _  )     = throwError "Can not translate pi expressions to STG."
genExp (ELet bs e)     = genLet bs e
genExp (EVar i)        = genVar i
genExp (EApp f x)      = genApp f [x]
genExp (ELam (i, _) e) = genLam [i] e
genExp (EPrim po xs)   = genPrim po xs

--------------------------------------------------------------------------------
-- Constants and Literals
--------------------------------------------------------------------------------

-- | STG code for constant expressions.
-- |
-- | Constructor constants are translated like constructor applications
-- | with no arguments. See `genAppCon` for constructor saturation.
-- | 
-- | Since no type level code is translated to STG, universes have no
-- | STG translation.
genConst :: Const -> Trans STG.Exp
genConst (CLit l)    = return $ STG.EAtom $ STG.ALit $ toLit l
genConst c@(CCon _)  = genApp (EConst c) []
genConst CUniv       = throwError "Can not translate universes."
genConst (CPrimUniv) = throwError "Can not translate primitive universes."
genConst (CGlobal n) = return $ toVar n
genConst (CPrim p)   = throwError "Can not translate primitive types."

toLit :: Lit -> STG.Lit
toLit (LInt i) = STG.LInt i

--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------

-- | STG code for applications.
-- | 
-- | Collects nested applications to a single application with multiple
-- | arguments for more efficient STG code.
-- |
-- | Constructor application is special cased, because in STG constructors
-- | must always be saturated.
genApp :: Exp -> [Exp] -> Trans STG.Exp
genApp (EApp f x)         xs = genApp f (x:xs)
genApp (EConst (CCon cn)) xs = genAppCon cn xs
genApp f                  xs = genAppFun f xs

-- | STG code for function applications.
-- |
-- | Because STG only allows variables (atoms) in applications,
-- | a let binding is created which binds the function and the arguments.
genAppFun :: Exp -> [Exp] -> Trans STG.Exp
genAppFun f xs = do
  -- create names for the function and the arguments
  fn <- freshName
  xn <- freshNames $ length xs
  -- create bindings for the arguments
  bs <- mapM toBind $ (fn, f) : zip xn xs
  -- assemble let
  return $ STG.ELet bs $ STG.EApp fn $ fmap STG.AVar xn

-- | STG code for constructor application.
-- |
-- | Constructor application must be saturated, so the arity of the
-- | constructor is checked. If too few arguments are supplied, a lambda
-- | is created which binds the additional arguments.
-- |
-- | Produces illegal code if the constructor is applied too many arguments.
-- | Make sure to filter out such code during type checking.
-- |
-- | Since in STG constructors can only be applied to atoms, a let binding
-- | is created which binds the arguments.
genAppCon :: Name -> [Exp] -> Trans STG.Exp
genAppCon cn xs = do
  c  <- lookupCon cn
  -- create names for arguments and saturating arguments
  xn <- freshNames $ length xs
  yn <- freshNames $ conArity c - length xs
  -- create saturated constructor object
  on <- freshName
  let obj = STG.Con (conIndex c) $ fmap STG.AVar (xn ++ yn)
  let sat = STG.ELet [STG.Bind on obj] $ toVar on
  -- if unsaturated, create lambda
  inner <- if null yn then return sat else do
    fn <- freshName
    return $ STG.ELet [STG.Bind fn $ STG.Fun yn sat] $ toVar fn
  -- now bind arguments
  bs <- mapM toBind $ zip xn xs
  return $ STG.ELet bs inner
  
--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

genVar :: Ident -> Trans STG.Exp
genVar  = return . toVar . toName

-- | Lifts a variable name into a STG expression.
toVar :: Name -> STG.Exp
toVar = STG.EAtom . STG.AVar

toName :: Ident -> Name
toName (Ident _ i) = "_u" ++ show i

--------------------------------------------------------------------------------
-- Let and Lambda Bindings
--------------------------------------------------------------------------------

-- | STG code for let bindings.
genLet :: [(Ident, Exp, Type)] -> Exp -> Trans STG.Exp
genLet ds e = do
  bs <- mapM toBind $ fmap (\(i, e, _) -> (toName i, e)) ds
  et <- genExp e
  return $ STG.ELet bs et

-- | STG code for lambdas. Since lambdas are heap objects, a STG let binding
-- | is created: let f = \x1 ... xn -> e in f
-- |
-- | Nested lambdas are collected into a lambda with multiple arguments for
-- | more efficient STG code.
genLam :: [Ident] -> Exp -> Trans STG.Exp
genLam is (ELam (i, _) e) = genLam (is ++ [i]) e
genLam is e               = do
  fn <- freshName
  et <- genExp e
  let b = STG.Bind fn $ STG.Fun (fmap toName is) et
  return $ STG.ELet [b] $ STG.EAtom $ STG.AVar fn

-- | Creates bindings for expressions. Lazyness is achieved by
-- | creating thunks.
toBind :: (STG.Var, Exp) -> Trans STG.Bind
toBind (n,e) = (STG.Bind n . STG.Thunk) <$> genExp e

--------------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------------

genPrim :: PrimOp -> [Exp] -> Trans STG.Exp
genPrim po xs = STG.EPrim (toPrimOp po) <$> mapM genExp xs

toPrimOp :: PrimOp -> STG.PrimOp
toPrimOp (PPlus p)         = STG.PPlus (toPrim p)
toPrimOp (PMult p)         = STG.PMult (toPrim p)
toPrimOp (PDiv p)          = STG.PDiv (toPrim p)
toPrimOp (PMinus p)        = STG.PMinus (toPrim p)
toPrimOp (PEq p)           = STG.PEq (toPrim p)
toPrimOp (PLt p)           = STG.PLt (toPrim p)
toPrimOp (PLte p)          = STG.PLte (toPrim p)
toPrimOp (PGt p)           = STG.PGt (toPrim p)
toPrimOp (PGte p)          = STG.PGte (toPrim p)
toPrimOp (PForeign v ps p) = STG.PForeign v (fmap toPrim ps) (toPrim p)

toPrim :: Prim -> STG.Prim
toPrim PInt = STG.PInt