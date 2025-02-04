{-# LANGUAGE DeriveGeneric #-}

module Model.SystemState (SystemState(..)) where

import Data.Map (Map) --Map dunciona como un Diccionario <clave, valor> (O(log n)) Buscar,Insertar,Eliminar
import Model.TimeSlot
import Model.Worker
import Model.Task
import Model.Resource
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

type ResourceUsage = Map Resource (Map Day (Map TimeSlot Int))
-- Recurso 1 (3 martillos) está ocupado el día 2023-10-01 en el slot 9-12: 2 unidades.

data SystemState = SystemState
  { workersSchedule     :: Map Worker [TimeSlot], -- Slots ocupados por trabajador
    resourcesUsage   :: ResourceUsage, -- Uso de recursos por día y timeSlot
    assignedTasks    :: Map Task (TimeSlot, [Worker], [(Resource, Int)]),  
    unassignedTasks  :: [Task]  -- Tareas que quedan sin asignar           
  } deriving (Show, Generic)

instance FromJSON SystemState --Autoderivado
instance ToJSON SystemState