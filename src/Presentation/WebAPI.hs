{-# LANGUAGE OverloadedStrings #-}

module Presentation.WebAPI where

import Web.Scotty
import Network.HTTP.Types (status200, status400)
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text.Lazy (pack)
import Network.Wai (Middleware, Application)  -- Importar Middleware y Application
import Network.Wai.Middleware.Cors (simpleCors)  -- Middleware CORS simple
import Network.Wai.Middleware.Static (staticPolicy, addBase, (>->))
--import Core.Scheduler.Greedy (scheduleTasks)
import Model.SystemState (SystemState(..))
import Model.Task (Task(..))
import Model.Worker (Worker(..))
import Data.Set (Set)
import qualified Data.Set as Set

-- Estado global compartido
type AppState = IORef SystemState

-- Iniciar servidor
runServer :: AppState -> IO ()
runServer stateRef = scotty 3000 $ do
  middleware customMiddleware -- Aplicar middleware
  routes stateRef -- Definir endpoints
  middleware $ staticPolicy (addBase "static")  -- Sirve archivos estáticos
  get "/" $ file "static/index.html"           -- Sirve el HTML principal

-- Middleware para CORS
customMiddleware :: Middleware
customMiddleware = simpleCors

-- Definir rutas
routes :: AppState -> ScottyM ()
routes stateRef = do
  --Tasks
  get "/api/tasks" $ getTasksHandler stateRef
  post "/api/tasks" $ postTaskHandler stateRef
  delete "/api/tasks/:id" $ deleteTaskHandler stateRef
  --Workers
  get "/api/workers" $ getWorkersHandler stateRef
  post "/api/workers" $ postWorkerHandler stateRef
  delete "/api/workers/:id" $ deleteWorkerHandler stateRef
  --Schedule
  post "/api/schedule" $ scheduleHandler stateRef

-- Handlers
getTasksHandler :: AppState -> ActionM ()
getTasksHandler stateRef = do
  state <- liftIO $ readIORef stateRef
  json (Set.toList (tasks state) :: [Task])

postTaskHandler :: AppState -> ActionM ()
postTaskHandler stateRef = do
  taskData <- jsonData :: ActionM Task
  liftIO $ modifyIORef' stateRef $ \state -> 
    let newId = nextTaskId state
        newTask = taskData { taskId = newId }
    in state { 
        tasks = Set.insert newTask (tasks state),
        nextTaskId = (nextTaskId state + 1)
      }
  json ("Tarea añadida con ID " :: String)

deleteTaskHandler :: AppState -> ActionM ()
deleteTaskHandler stateRef = do
  taskIdParam <- pathParam "id"
  liftIO $ modifyIORef' stateRef $ \state -> 
    state { tasks = Set.filter (\t -> taskId t /= taskIdParam) (tasks state) }
  json $ "Tarea #" ++ show taskIdParam ++ " eliminada"

deleteWorkerHandler :: AppState -> ActionM ()
deleteWorkerHandler stateRef = do
  workerIdParam <- pathParam "id"
  liftIO $ modifyIORef' stateRef $ \state -> 
    state { workers = Set.filter (\w -> workerId w /= workerIdParam) (workers state) }
  json $ "Trabajador #" ++ show workerIdParam ++ " eliminado"

getWorkersHandler :: AppState -> ActionM ()
getWorkersHandler stateRef = do
  state <- liftIO $ readIORef stateRef
  json (Set.toList (workers state) :: [Worker])

postWorkerHandler :: AppState -> ActionM ()
postWorkerHandler stateRef = do
  workerData <- jsonData :: ActionM Worker
  liftIO $ modifyIORef' stateRef $ \state -> 
    let newId = nextWorkerId state
        newWorker = workerData { workerId = newId }
    in state { 
        workers = Set.insert newWorker (workers state),
        nextWorkerId = (nextWorkerId state + 1)
      }
  json ("Trabajador añadido con ID " :: String)

scheduleHandler :: AppState -> ActionM ()
scheduleHandler stateRef = do
  json ("Tareas programadas" :: String)