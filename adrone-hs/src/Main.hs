module Main where

import Data.Diary
import System.Environment (getArgs)


-- | Branching operation by first argument
main :: IO ()
main = do
  x <- getArgs
  app x

-- | App's body
app :: [String] -> IO ()
app ("insert":_) = do
  prepareAdroneDB

app ["list"] = do
  prepareAdroneDB

app [] = error "This program needs specifying startup target"
