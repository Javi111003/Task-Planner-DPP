{-# LANGUAGE DeriveGeneric #-}
module Model.Task (Task(..)) where

import Data.Time.Calendar (Day)
import Model.Resource
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)

data PriorityType = Low | Medium | High deriving (Show, Eq, Ord, Generic)
instance FromJSON PriorityType --Autoderivado
instance ToJSON PriorityType


-- Tarea
-- taskId : para identificar univocamente cada tarea
-- description: descripcion de la tarea (para mostrar)
-- estimatedTime: tiempo que tomara realizar la tarea
-- requiredSkills: lista de habilidades necesarias para la tarea

data Task = Task {
    taskId :: Int,
    description :: String,
    estimatedTime :: Int, 
    requiredSkills :: [String],

    -- Propiedades no usadas(de momento)
    
    startDate :: Day, 
    deadline :: Day, 
    priority :: PriorityType, 
    requiredResources :: [(Resource, Int)], 
    dependencies :: [Task] 

} deriving (Show, Eq, Ord, Generic)

instance FromJSON Task --Autoderivado
instance ToJSON Task

instance FromJSONKey Task
instance ToJSONKey Task