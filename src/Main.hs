{-# LANGUAGE RecordWildCards #-}

module Main where

import Model.Task (Task(..))
import Model.Worker
import Model.Resource (Resource(..), ResourceType(..))
--import Persistence.Persistence (saveTasks, loadTasks)
import Data.Time.Calendar (fromGregorian)
import Presentation.WebAPI (runServer)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Model.SystemState (SystemState(..))
import Utils.CRUD (emptySet)

main :: IO ()
main = do

  let initialState = SystemState emptySet emptySet 1 1 Map.empty Map.empty Map.empty [] -- Crear estado inicial en memoria
  -- Crear estado inicial en memoria
  stateRef <- newIORef initialState
  
  -- Iniciar servidor
  runServer stateRef
    -- Probando guardar y cargar tareas, (PERSISTENCIA)
    -- let resources = Resource 1 "RECURSO1" 3 Limited
    -- let path = "tasktest.json"
    -- Guardar una tarea en el archivo JSON
    -- saveTasks path [Task 1 "Tarea 1" 1 (fromGregorian 2023 10 1) (fromGregorian 2023 10 10) Low ["Skill1"] [(resources, 1)] []]
    -- putStrLn $ "Tarea guardada en " ++ path
    -- Cargar las tareas desde el archivo JSON
    -- maybeTasks <- loadTasks path 
    -- case maybeTasks of
    --     Just tasks -> mapM_ (putStrLn . description) tasks
    --     Nothing -> putStrLn "No se pudieron cargar las tareas."
