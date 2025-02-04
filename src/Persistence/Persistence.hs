module Persistence.Persistence where

import Model.SystemState (SystemState)
import Model.Task (Task)
import Model.Resource (Resource)
import Model.Worker (Worker)
--import Data.Aeson (encodeFile, decodeFileStrict)
-import System.IO (FilePath)

--saveState :: FilePath -> SystemState -> IO ()
--saveState path = encodeFile path

--loadState :: FilePath -> IO (Maybe SystemState)
--loadState path = decodeFileStrict path

--saveTasks :: FilePath -> [Task] -> IO ()
--saveTasks path = encodeFile path

--loadTasks :: FilePath -> IO (Maybe [Task])
--loadTasks path = decodeFileStrict path

--saveResources :: FilePath -> [Resource] -> IO ()
--saveResources path = encodeFile path

--loadResources :: FilePath -> IO (Maybe [Resource])
--loadResources path = decodeFileStrict path

--saveWorkers :: FilePath -> [Worker] -> IO ()
--saveWorkers path = encodeFile path

--loadWorkers :: FilePath -> IO (Maybe [Worker])
--loadWorkers path = decodeFileStrict path