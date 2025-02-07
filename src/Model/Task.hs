{-# LANGUAGE DeriveGeneric #-}
module Model.Task (Task(..)) where

import Data.Time.Calendar (Day)
import Model.Resource
import GHC.Generics (Generic)
import Model.TimeSlot (TimeSlot(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set

data PriorityType = Low | Medium | High deriving (Show, Eq, Ord, Generic)
instance FromJSON PriorityType 
instance ToJSON PriorityType

-- Tarea
-- taskId : para identificar univocamente cada tarea
-- description: descripcion de la tarea (para mostrar)
-- estimatedTime: tiempo que tomara realizar la tarea
-- requiredSkills: conjunto de habilidades necesarias para la tarea
data Task = Task {
    taskId :: Int,
    description :: String,
    estimatedTime :: Int, 
    requiredSkills :: Set String,
    startDate :: Day, 
    deadline :: Day, 
    priority :: PriorityType, 
    requiredResources :: Map Resource Int,
    dependencies :: Set Task,
    toDoSlots :: Set TimeSlot
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Task 
instance ToJSON Task
instance FromJSONKey Task
instance ToJSONKey Task