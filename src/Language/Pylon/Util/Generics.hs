-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Pylon.Util.Generics
-- Copyright   :  Lukas Heidemann 2014
-- License     :  BSD
--
-- Maintainer  :  lukasheidemann@gmail.com
-- Stability   :  experimental
-- Portability :  semi-portable

--------------------------------------------------------------------------------
module Language.Pylon.Util.Generics where
--------------------------------------------------------------------------------

import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data
import Data.Generics.Str

--------------------------------------------------------------------------------
-- Traversals
--------------------------------------------------------------------------------

transformCtx :: (Monad m, Data on) => (on -> [m on -> m on]) -> (on -> m on) -> on -> m on
transformCtx gs f x = cur' >>= f . gen . strUn where
  (cur, gen)    = uniplate x
  (strL, strUn) = strStructure cur
  gs'           = gs x ++ repeat id
  cur' = sequence                -- sequence and convert back to str
    $ zipWith ($) gs'            -- apply contexts
    $ fmap (transformCtx gs f)   -- transform children
    $ strL                -- str to list
