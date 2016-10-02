-- | Definitions of exception instance
module Data.AdroneException
  ( IOException' (..)
  ) where

import Control.Monad.Catch (Exception)

-- | Throwable version IOException
data IOException' = IOException' String deriving (Show)
instance Exception IOException'
