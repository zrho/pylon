--------------------------------------------------------------------------------
-- |
-- Module      : Language.Pylon.Core.Check
-- Copyright   : (c) 2014 Lukas Heidemann
-- License     : BSD
-- Maintainer  : lukasheidemann@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Type checker for Pylon Core.
--
-- TODO: check for over-specialization, as in the Idris code:
-- > unsafe : (a : Type) -> (x : a) -> a
-- > unsafe Bool x = False
-- Currently this type checks unsafe code!
--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Pylon.Core.Check
  ( TypeError
  , Locals
  , checkExp
  , checkBind
  ) where
--------------------------------------------------------------------------------

import Language.Pylon.Util
import Language.Pylon.Util.Name
import Language.Pylon.Util.Error
import Language.Pylon.Core.AST
import Language.Pylon.Core.Monad
import Language.Pylon.Core.Util

import Control.Monad              (unless)
import Control.Monad.Except       (MonadError)
import Control.Monad.State.Class  (MonadState, get, put)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Applicative hiding (Const)
import Control.Concurrent.Supply

import           Data.Foldable (forM_)
import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Monoid   (mempty, (<>))

--------------------------------------------------------------------------------
-- API: Type Checker
--------------------------------------------------------------------------------

type TypeError = CtxError Ann String
type Locals    = Map Ident Type

-- |
-- Type checks a Pylon Core expression and returns its inferred type, if the
-- expression is type correct.
checkExp :: Supply -> Program -> Locals -> Exp -> Either TypeError Type
checkExp s p l e = runCheck s p l (tcExp e)

-- |
-- Type checks a Pylon Core binding.
checkBind :: Supply -> Program -> Name -> Bind -> Either TypeError ()
checkBind s p n b = runCheck s p mempty (tcBind n b)

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

newtype Check a = Check
  { fromCheck :: RWSE CheckReader () CheckState CheckError a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState  CheckState
  , MonadReader CheckReader
  , MonadError  CheckError
  )

type CheckError  = TypeError
type CheckState  = Supply
data CheckReader = CheckReader
  { crFree    :: Map Ident Type
  , crProgram :: Program
  }

instance MonadProgram Check where
  getProgram = asks crProgram

instance MonadSupply Check where
  getSupply = get
  putSupply = put

runCheck :: Supply -> Program -> Locals -> Check a -> Either CheckError a
runCheck s p l go = runRWSE (fromCheck go) (CheckReader l p) s

freshIdent :: Check Ident
freshIdent = IGen "Check" <$> supplyId

ensureEq :: Exp -> Exp -> String -> Check ()
ensureEq ex ac msg
  | ex == ac  = return ()
  | otherwise = throwError' $ unlines
    [ msg, "Expected: " ++ show ex, "Actual: " ++ show ac ]

--------------------------------------------------------------------------------
-- Locals and Program Context
--------------------------------------------------------------------------------

-- | Introduces locals into scope.
withFree :: [(Ident, Type)] -> Check a -> Check a
withFree vs = local $ \s -> s { crFree = M.fromList vs <> crFree s }

-- | Looks up the type of a local.
lookupFree :: Ident -> Check Type
lookupFree i = asks crFree >>= \vs -> case M.lookup i vs of
  Just v  -> return v
  Nothing -> throwError' $ "Unknown local variable: " ++ show i

--------------------------------------------------------------------------------
-- TC: Bindings and Matches
--------------------------------------------------------------------------------

-- |
-- Checks a binding by checking its matches.
tcBind :: Name -> Bind -> Check ()
tcBind _ (Bind Nothing _)     = return ()
tcBind n (Bind (Just []) _)   = throwError' $ "Binding without any matches: " ++ n
tcBind n (Bind (Just ms) _)
  | arityMismatch ms          = throwError' $ "Arity mismatch in binding: " ++ n
  | otherwise                 = mapM_ (tcMatch n) ms

-- |
-- Determines, whether there is an mismatch in arity in the given matches.
arityMismatch :: [Match] -> Bool
arityMismatch = not . pairwise (==) . fmap matchArity

-- |
-- Checks if the match is well-formed, then tries to match the types of the
-- left- and right-hand side.
tcMatch :: Name -> Match -> Check ()
tcMatch name m@(Match vs lhs rhs) = withFree vs $ do
  unless (isLhsForm name lhs) $ throwError' $ "Illegal left hand side in match: " ++ show m
  lhst <- tcExp lhs
  rhst <- tcExp rhs
  ensureEq (hnf lhst) (hnf rhst) $ "Type mismatch in match: " ++ show m

-- |
-- Checks whether the expression can appear on a left hand side.
-- todo: this is not complete yet. it accepts too much.
isLhsForm :: Name -> Exp -> Bool
isLhsForm n (EApp f _) = isLhsForm n f
isLhsForm n (EFree m)  = ISource n == m
isLhsForm _ _          = False

--------------------------------------------------------------------------------
-- TC: Expressions
--------------------------------------------------------------------------------

-- |
-- Type checks an expression by dispatching to the respective functions.
tcExp :: Exp -> Check Type
tcExp (EConst c )   = tcConst c
tcExp (EApp f x )   = tcApp f x
tcExp (ELam t e)    = tcLam t e
tcExp (EPi t e )    = tcPi t e
tcExp (ELet t b e)  = tcLet t b e
tcExp (EFree i)     = tcFree i
tcExp (EPrim po xs) = tcPrim po xs
tcExp (EAnn a x)    = tcAnn a x
tcExp (ELocal _)    = throwError' $ "Unopened local while type checking."
tcExp (EHole _)     = throwError' $ "Holes can not be type checked."
tcExp (EBind _ _)   = error "Unmatched binder type."

-- |
-- Type checks an expression, then converts its type to head normal form.
tcExpNF :: Exp -> Check Type
tcExpNF = fmap hnf . tcExp

-- |
-- Logs the annotation for better error messages, then typechecks the expression.
tcAnn :: Ann -> Exp -> Check Type
tcAnn a e = errorContext a $ tcExp e

-- |
-- Typechecks constants.
tcConst :: Const -> Check Type
tcConst (CLit    lit ) = return $ litType lit
tcConst (CCon    name) = conType <$> lookupCon name
tcConst CUniv          = return $ EConst CUniv
tcConst CPrimUniv      = return $ EConst CUniv
tcConst (CPrim _     ) = return $ EConst CPrimUniv

-- |
-- Checks if the type of the head is a function (pi) type; then opens the return
-- type of the function type with the argument, if the argument type matches.
tcApp :: Exp -> Exp -> Check Type
tcApp f x = tcExpNF f >>= \ft -> case ft of
  EPi tx rt -> do
    xt <- tcExpNF x
    ensureEq (hnf tx) xt $ "Bad argument type."
    return $ open x rt
  _ -> throwError' "Application to non-function."

-- |
-- Opens the body with a new identifier with the bound type, then constructs
-- a pi type from the bound type and the inferred result type. 
tcLam :: Type -> Exp -> Check Type
tcLam t e = do
  i  <- freshIdent
  rt <- withFree [(i, t)] $ tcExp $ open (EFree i) e
  return $ EPi t (close i rt)

-- |
-- Opens the body with a new identifier with the bound type; if it type checks,
-- returns Universe.
tcPi :: Type -> Exp -> Check Type
tcPi t e = do
  i <- freshIdent
  _ <- withFree [(i, t)] $ tcExp $ open (EFree i) e
  return $ EConst CUniv

-- |
-- Type checks the bound expression and checks if its type matches with the
-- given one. If so, open the body with a new identifier of the stated type and
-- infer its type.
tcLet :: Type -> Exp -> Exp -> Check Type
tcLet t b e = do
  i  <- freshIdent
  bt <- tcExp b
  ensureEq (hnf t) (hnf bt) $ "Bad typed let binding."
  withFree [(i, t)] $ tcExp $ open (EFree i) e

-- |
-- Lookup the type of the free variable in scope.
tcFree :: Ident -> Check Type
tcFree = lookupFree

--------------------------------------------------------------------------------
-- TC: Primitive and Literals
--------------------------------------------------------------------------------

-- | Check primitive application.
-- todo: add IO
tcPrim :: PrimOp -> [Exp] -> Check Type
tcPrim p xs = do
  let (as, r) = primType p
  unless (length as == length xs) $ throwError' "Under/oversaturated primitive application."
  forM_ (zip as xs) $ \(a, x) -> do
    xt <- fmap hnf $ tcExp x
    ensureEq (EConst $ CPrim a) xt $ "Mismatch in primitive type."
  return $ EConst $ CPrim r

primType :: PrimOp -> ([Prim], Prim)
primType op = case op of
  PPlus p         -> binOp p
  PMult p         -> binOp p
  PDiv p          -> binOp p
  PMinus p        -> binOp p
  PEq p           -> binOp p
  PLt p           -> binOp p
  PLte p          -> binOp p
  PGt p           -> binOp p
  PGte p          -> binOp p
  PForeign _ ps p -> (ps, p)
  where binOp p = ([p, p], p)

litType :: Lit -> Type
litType (LInt _) = EConst $ CPrim PInt
