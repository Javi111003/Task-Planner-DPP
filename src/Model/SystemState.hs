{-# LANGUAGE DeriveGeneric #-}
module Model.SystemState (SystemState(..)) where

import Data.Map (Map) 
import Model.TimeSlot
import Model.Worker
import Model.Task
import Model.Resource
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import qualified Data.Set as Set

type ResourceUsage = Map Resource (Map Day (Map TimeSlot Int))
-- Recurso 1 (3 martillos) está ocupado el día 2023-10-01 en el slot 9-12: 2 unidades. 

data SystemState = SystemState {
    tasks           :: Set Task, -- Tareas a organizar
    workers         :: Set Worker, -- Trabajadores disponibles
    nextTaskId      :: Int,
    nextWorkerId    :: Int,
    workersSchedule :: Map Worker (Set TimeSlot),
    resourcesUsage  :: ResourceUsage, -- Uso de recursos por día y timeSlot
    assignedTasks   :: Map Task (TimeSlot, Set Worker, Map Resource Int),  
    unassignedTasks :: Set Task  -- Tareas que quedan sin asignar           
} deriving (Show, Generic)

instance FromJSON SystemState 
instance ToJSON SystemState