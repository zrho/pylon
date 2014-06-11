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
module Language.Pylon.Core.STG where
--------------------------------------------------------------------------------

import           Language.Pylon.Util
import           Language.Pylon.Util.Name
import           Language.Pylon.Core.AST
import qualified Language.Pylon.STG.AST  as STG

import Control.Monad              (replicateM)
import Control.Monad.State.Class  (MonadState, modify, gets)
import Control.Monad.Trans.State  (StateT, runStateT)
import Control.Monad.Trans.Class  (lift)
import Control.Applicative hiding (Const)

import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

newtype Trans a = Trans { fromTrans :: StateT TransState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState TransState)

data TransState = TransState
  { tsProgram :: Program
  , tsNames   :: Int
  , tsLocals  :: [STG.Var]
  } deriving (Eq, Show)

instance MonadName [Char] Trans where
  freshName = do
    n <- gets tsNames
    modify $ \s -> s { tsNames = n + 1 }
    return $ '_' : show n

runTrans :: Trans a -> Program -> Either String a
runTrans go p = fmap fst $ runStateT (fromTrans go) $ TransState p 0 []

-- | Looks up a constructor in the program.
lookupCon :: Name -> Trans Con
lookupCon name = do
  dat <- gets $ prData . tsProgram
  case M.lookup name dat of
    Just c  -> return c
    Nothing -> failTrans $ "No such constructor: " ++ name

-- | Fails the translation with an error message.
failTrans :: String -> Trans a
failTrans = Trans . lift . Left

--------------------------------------------------------------------------------
-- Locals
--------------------------------------------------------------------------------

withLocals :: [STG.Var] -> Trans a -> Trans a
withLocals vs go = do
  ws <- gets tsLocals
  modify $ \s -> s { tsLocals = reverse vs ++ ws }
  x <- go
  modify $ \s -> s { tsLocals = ws }
  return x

getLocal :: Int -> Trans STG.Var
getLocal i = do
  vs <- gets tsLocals
  case safeIndex vs i of
    Just v  -> return v
    Nothing -> failTrans $ "Local out of bounds: " ++ show i

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | STG code for Pylon Core expressions.
-- |
-- | Since no type level code is translated to STG, pi expressions have no
-- | STG translation.
genExp :: Exp -> Trans STG.Exp
genExp (EConst c )    = genConst c
genExp (EPi _ _  )    = failTrans "Can not translate pi expressions to STG."
genExp (ELet bs e)    = genLet bs e
genExp (ECase as d e) = genCase as d e
genExp (ELocal i)     = genLocal i
genExp (EApp f x)     = genApp f [x]
genExp (ELam _ e)     = genLam 1 e

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
genConst CUniv       = failTrans "Can not translate universes."
genConst (CGlobal n) = return $ toVar n

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

genLocal :: Int -> Trans STG.Exp
genLocal = fmap toVar . getLocal

-- | Lifts a variable name into a STG expression.
toVar :: Name -> STG.Exp
toVar = STG.EAtom . STG.AVar

--------------------------------------------------------------------------------
-- Let and Lambda Bindings
--------------------------------------------------------------------------------

-- | STG code for let bindings.
genLet :: [(Exp, Type)] -> Exp -> Trans STG.Exp
genLet ds e = do
  let es = fmap fst ds
  vs <- freshNames $ length es
  bs <- withLocals vs $ mapM toBind $ zip vs es
  et <- withLocals vs $ genExp e
  return $ STG.ELet bs et

-- | STG code for lambdas. Since lambdas are heap objects, a STG let binding
-- | is created: let f = \x1 ... xn -> e in f
-- |
-- | Nested lambdas are collected into a lambda with multiple arguments for
-- | more efficient STG code.
genLam :: Int -> Exp -> Trans STG.Exp
genLam n (ELam _ e) = genLam (n + 1) e
genLam n e          = do
  fn <- freshName
  vs <- freshNames n
  et <- withLocals vs $ genExp e
  let b = STG.Bind fn $ STG.Fun vs et
  return $ STG.ELet [b] $ STG.EAtom $ STG.AVar fn

-- | Creates bindings for expressions. Lazyness is achieved by
-- | creating thunks.
toBind :: (STG.Var, Exp) -> Trans STG.Bind
toBind (n,e) = (STG.Bind n . STG.Thunk) <$> genExp e

--------------------------------------------------------------------------------
-- Case Expressions
--------------------------------------------------------------------------------

-- | STG code for case expressions.
-- | Note that all case expressions in Pylon Core are algebraic.
genCase :: [(Pat, Exp)] -> Exp -> Exp -> Trans STG.Exp
genCase as de e = do
  et <- genExp e
  at <- mapM toAAlt as
  dn <- freshName
  dt <- withLocals [dn] $ genExp de
  let def = STG.Default (Just dn) dt
  return $ STG.ECase (STG.AAlts at def) et

-- | STG algebraic alternatives.
toAAlt :: (Pat, Exp) -> Trans STG.AAlt
toAAlt (PCon c n, e) = do
  vs <- freshNames n
  et <- withLocals vs $ genExp e
  ct <- fmap conIndex $ lookupCon c
  return $ STG.AAlt ct vs et
