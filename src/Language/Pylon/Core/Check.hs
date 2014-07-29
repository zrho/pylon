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
-- Requires an already scoped program.
--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Pylon.Core.Check where
--------------------------------------------------------------------------------

import Language.Pylon.Util
import Language.Pylon.Util.Subst
import Language.Pylon.Util.Name
import Language.Pylon.Core.AST
import Language.Pylon.Core.Monad
import Language.Pylon.Core.Util

import Control.Arrow
import Control.Monad              (unless, void)
import Control.Monad.Error.Class  (MonadError, throwError, catchError)
import Control.Monad.State.Class  (MonadState, gets, modify)
import Control.Monad.Trans.State  (StateT, runStateT)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Applicative hiding (Const)

import           Data.Foldable (toList, forM_, foldMap)
import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Set      (Set)
import qualified Data.Set as S
import           Data.Monoid   ((<>))
import           Data.Maybe    (fromMaybe)
import           Data.List     (sort)

import Debug.Trace

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

newtype Check a = Check { fromCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)

data CheckState = CheckState
  { csFree    :: Map Ident Type
  , csProgram :: Program
  , csName    :: Int
  } deriving (Eq, Show)

instance MonadProgram Check where
  getProgram = gets csProgram

instance MonadName Ident Check where
  freshName = do
    n <- gets csName
    modify $ \s -> s { csName = n + 1 }
    return $ IGen "Check" n

runCheck :: Check a -> Program -> Map Ident Type -> Either String a
runCheck go p loc = fmap fst $ runStateT (fromCheck go) $ CheckState loc p 0

--------------------------------------------------------------------------------
-- Locals and Program Context
--------------------------------------------------------------------------------

-- | Introduces locals into scope.
withFree :: [(Ident, Type)] -> Check a -> Check a
withFree vs go = do
  ws <- gets csFree
  modify $ \s -> s { csFree = M.fromList vs <> ws }
  forM_ vs $ \(v, t) -> tcExp t
  x <- go
  modify $ \s -> s { csFree = ws }
  return x

-- | Looks up the type of a local.
lookupFree :: Ident -> Check Type
lookupFree i = gets csFree >>= \vs -> case M.lookup i vs of
  Just v  -> return v
  Nothing -> throwError $ "Unknown local variable: " ++ show i

--------------------------------------------------------------------------------
-- Programs
--------------------------------------------------------------------------------

tcProgram :: Check ()
tcProgram = do
  p <- gets csProgram
  mapM_ tcBind $ M.toList $ prBind p

tcBind :: (Name, Bind) -> Check ()
tcBind (name, Bind Nothing t) = return ()
tcBind (name, Bind (Just ms) t) = do
  -- todo check lhs count
  mapM_ (tcMatch name) ms

--------------------------------------------------------------------------------
-- Matches
--------------------------------------------------------------------------------

tcMatch :: Name -> Match -> Check ()
tcMatch name (Match vs lhs rhs) =
  withFree vs $ do
    unless (isLhsForm name lhs) $ throwError $ "Illegal left hand side form in binding: " ++ name
    lhst <- tcExp lhs
    rhst <- tcExp rhs
    ensureEq (hnf lhst) (hnf rhst) $ "Type mismatch in binding: " ++ name

isLhsForm :: Name -> Exp -> Bool
isLhsForm n (EApp f x) = isLhsForm n f
isLhsForm n (EFree m)  = ISource n == m
isLhsForm _ _          = False

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

tcExp :: Exp -> Check Type
tcExp (EConst c )   = tcConst c
tcExp (EApp f x )   = tcApp f x
tcExp (ELam t e)    = tcLam t e
tcExp (EPi t e )    = tcPi t e
tcExp (ELet t b e)  = tcLet t b e
tcExp (EFree i)     = tcFree i
tcExp (ELocal i)    = tcLocal i
tcExp (EPrim po xs) = tcPrim po xs
-- todo: holes

tcExpNF :: Exp -> Check Type
tcExpNF = fmap hnf . tcExp

tcConst :: Const -> Check Type
tcConst (CLit    lit ) = return $ litType lit
tcConst (CCon    name) = conType <$> lookupCon name
tcConst CUniv          = return $ EConst CUniv
tcConst CPrimUniv      = return $ EConst CUniv
tcConst (CPrim p     ) = return $ EConst CPrimUniv

tcApp :: Exp -> Exp -> Check Type
tcApp f x = tcExpNF f >>= \ft -> case ft of
  EPi tx rt -> do
    xt <- tcExpNF x
    ensureEq (hnf tx) xt $ "Bad argument type in " ++ show (EApp f x)
    return $ open x rt
  _ -> throwError "Application to non-function."

tcLam :: Type -> Exp -> Check Type
tcLam t e = do
  i  <- freshName
  rt <- withFree [(i, t)] $ tcExp $ open (EFree i) e
  return $ EPi t (close i rt)

tcPi :: Type -> Exp -> Check Type
tcPi t e = do
  i <- freshName
  _ <- withFree [(i, t)] $ tcExp $ open (EFree i) e
  return $ EConst CUniv

tcLet :: Type -> Exp -> Exp -> Check Type
tcLet t b e = do
  i  <- freshName
  bt <- tcExp b
  ensureEq (hnf t) (hnf bt) $ "Bad typed let binding in: " ++ show (ELet t b e)
  tcExp $ open (EFree i) e

tcFree :: Ident -> Check Type
tcFree = lookupFree

tcLocal :: Index -> Check Type
tcLocal = error "todo"

--------------------------------------------------------------------------------
-- Primitive and Literals
--------------------------------------------------------------------------------

-- | Check primitive application.
-- todo: add IO
tcPrim :: PrimOp -> [Exp] -> Check Type
tcPrim p xs = do
  let (as, r) = primType p
  unless (length as == length xs) $ throwError "Under/oversaturated primitive application."
  forM_ (zip as xs) $ \(a, x) -> do
    xt <- fmap hnf $ tcExp x
    ensureEq (EConst $ CPrim a) xt $ "Mismatch in primitive type."
  return $ EConst $ CPrim r

primType :: PrimOp -> ([Prim], Prim)
primType p = case p of
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

--------------------------------------------------------------------------------

ensureEq :: Exp -> Exp -> String -> Check ()
ensureEq ex ac msg
  | ex == ac  = return ()
  | otherwise = throwError $ unlines
    [ msg, "Expected: " ++ show ex, "Actual: " ++ show ac ]
