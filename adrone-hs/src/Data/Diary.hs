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
  , insertMessage
  ) where

import Control.Monad (MonadPlus(mplus), liftM2)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.AdroneException
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Posix.Env (getEnv, getEnvDefault)
import qualified Data.Text as Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
AdroneDiary
  date    UTCTime default=CURRENT_TIME
  message Text
  deriving Show
|]


-- If you have $XDG_DATA_DIR, return "${XDG_DATA_DIR}/adronehs"
-- otherwise, return "${HOME}/.adronehs"
adroneDir :: (MonadIO m, MonadThrow m) => m FilePath
adroneDir = do
  maybeXdgDir <- liftIO $ getEnv "XDG_DATA_DIR"
  case maybeXdgDir of
    Just xdgDir -> return $ xdgDir ++ "/adronehs"
    Nothing     -> do
      maybeHomeDir <- liftIO $ getEnv "HOME"
      case maybeHomeDir of
        Just homeDir -> return $ homeDir ++ "/.adronehs"
        Nothing      -> throwM $ IOException' "Control.Diary.adroneDir: You must set $XDG_DATA_DIR or $HOME value"


-- adrone-hs.vim uses this file as sqlite db
adroneDBFileName :: FilePath
adroneDBFileName = "adrone.sqlite"


-- | Insert AdroneDiary to the app DB
insertMessage :: MonadIO m => Text -> m ()
insertMessage msg = do
  dir <- liftIO adroneDir
  liftIO $ createDirectoryIfMissing True dir
  let adroneDBFile = Text.pack $ dir ++ "/" ++ adroneDBFileName
  now <- liftIO $ getCurrentTime
  liftIO . runSqlite adroneDBFile $ do
    runMigration migrateAll
    insert $ AdroneDiary now msg
    return ()
