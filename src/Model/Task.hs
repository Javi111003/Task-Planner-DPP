module Model.Task (Task(..)) where

import Data.Time.Calendar (Day)
import Model.Resource

data PriorityType = Low | Medium | High deriving (Show, Eq)

data Task = Task {
    taskId :: Int,
    description :: String,
    estimatedTime :: Int, --Tiempo Estimado en Horas
    startDate :: Day, --Fecha en la que se debe iniciar la tarea
    deadline :: Day, --La tarea debe finalizar en esta fecha o antes
    priority :: PriorityType, --Indica que tan importante es la tarea, si dos tareas comienzan el mismo dia, se debe priorizar la de mayor prioridad
    requiredSkills :: [String],
    requiredResources :: [(Resource, Int)], --Cantidad de cada recurso necesario para realizar la tarea
    dependencies :: [Task] --Tareas que deben finalizar antes de que esta pueda comenzar(pendiente)
} deriving (Show, Eq)