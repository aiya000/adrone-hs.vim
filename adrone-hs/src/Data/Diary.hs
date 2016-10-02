{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | All type definition for adrone-hs.vim in here
module Data.Diary
  ( AdroneDiary (..)
  , migrateAll
  , prepareAdroneDB
  ) where

import Control.Monad (MonadPlus(mplus), liftM2)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.AdroneException
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Directory (doesFileExist)
import System.Posix.Env (getEnv, getEnvDefault)
import qualified Data.Text as Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
AdroneDiary
  date    UTCTime default=CURRENT_TIME
  message Text
  deriving Show
|]


-- If you have $XDG_DATA_DIR, return "${XDG_DATA_DIR}/adronehs/adrone.sqlite"
-- otherwise return "${HOME}/.adronehs/adrone.sqlite"
adroneDBFile :: (MonadIO m, MonadThrow m) => m Text
adroneDBFile = do
  maybeDir <- liftIO $ liftM2 mplus (getEnv "XDG_DATA_DIR") (getEnv "HOME")
  case maybeDir of
    Just dir -> return $ Text.pack dir <> "/adronehs/adrone.sqlite"
    Nothing  -> throwM $ IOException' "Control.Diary.adroneDBFile: You must set $HOME value"

-- | You must call this function before using functions in here
prepareAdroneDB :: IO ()
prepareAdroneDB = do
  dbFile <- adroneDBFile
  runSqlite dbFile $ runMigration migrateAll

-- | Insert AdroneDiary to adroneDBFile
insertMessage :: Text -> IO ()
insertMessage msg = adroneDBFile >>= \f -> runSqlite f $ do
  runMigration migrateAll
  now    <- liftIO getCurrentTime
  insert $ AdroneDiary now msg
  return ()
