module Main where

import Data.Diary
import System.Environment (getArgs)
import qualified Data.Text as Text


-- | Branching operation by first argument
main :: IO ()
main = app =<< getArgs

-- | App's body
app :: [String] -> IO ()
app ["insert", msg] = insertMessage $ Text.pack msg

app ["list"] = undefined

app [] = error "This program needs specifying startup target"
app xs = error $ "Got the unknowned arguments: " ++ show xs
