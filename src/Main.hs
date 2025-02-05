{-# LANGUAGE RecordWildCards #-}

module Main where

import Model.Task (Task(..))
import Model.Worker
import Model.Resource (Resource(..), ResourceType(..))
import Persistence.Persistence (saveTasks, loadTasks)
import Data.Time.Calendar (fromGregorian)
import Presentation.WebAPI (runServer)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Model.SystemState (SystemState(..))
import Utils.CRUD (emptySet)
import System.Environment (getArgs)
import System.Process (callCommand)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  let initialState = SystemState emptySet emptySet 1 1 Map.empty Map.empty Map.empty [] -- Crear estado inicial en memoria
  stateRef <- newIORef initialState
  
  -- Iniciar servidor
  _ <- forkIO $ runServer stateRef
  
  -- Abrir navegador web
  openBrowser "http://localhost:3000/index.html"
  
  -- Mantener el programa corriendo
  getLine
  return ()

openBrowser :: String -> IO ()
openBrowser url = do
  callCommand $ "start " ++ url