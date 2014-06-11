{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Pylon.Codegen.Monad
  ( Codegen ()
  , runCodegen
  , freshName
  , indent
  , top
  , tell
  ) where

-------------------------------------------------------------------------------

import Language.Pylon.Util.Name

import Control.Applicative
import Control.Monad              (liftM, ap)
import Control.Monad.State        (State, runState)
import Control.Monad.State.Class  (MonadState, gets, modify)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class (MonadWriter, pass, tell)

import Data.String                (IsString, fromString)
import Data.Maybe                 (listToMaybe)
import Data.Monoid                (Monoid, mempty, mappend)

-------------------------------------------------------------------------------

newtype Codegen a = Codegen
  { fromCodegen :: WriterT String (State CodegenState) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadWriter String)

data CodegenState = CodegenState
  { cgCode  :: String 
  , cgNames :: Int
  , cgTop   :: [Codegen ()]
  }

instance MonadName [Char] Codegen where
  freshName = do
    n <- gets cgNames
    modify $ \s -> s { cgNames = n + 1 }
    return $ "stg_" ++ show n

-------------------------------------------------------------------------------

runCodegen :: Codegen a -> String
runCodegen cg = snd $ fst $
  runState (runWriterT $ fromCodegen $ cg >> runTops) emptyCodegen

emptyCodegen :: CodegenState
emptyCodegen = CodegenState
  { cgCode   = ""
  , cgNames  = 0
  , cgTop    = []
  }

-------------------------------------------------------------------------------

runTops :: Codegen ()
runTops = gets cgTop >>= \t -> case t of
  []     -> return ()
  (t:ts) -> do
    modify $ \s -> s { cgTop = ts }
    t
    runTops

-------------------------------------------------------------------------------

-- | Inline string to code lifiting.
instance (a ~ ()) => IsString (Codegen a) where
  fromString = tell

instance (a ~ ()) => Monoid (Codegen a) where
  mempty      = return ()
  mappend m n = m >> n 

-------------------------------------------------------------------------------

-- | Indent a code block.
indent :: Codegen a -> Codegen a
indent = pass . fmap (\x -> (x, unlines . fmap ("  " ++) . lines))

-- | Add a top level declaration.
top :: Codegen () -> Codegen ()
top t = modify $ \s -> s { cgTop = t : cgTop s }