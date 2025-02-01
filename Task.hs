module Task (Task(..)) where

import Date
import Resource

data Task = Task {
    taskId :: Int,
    description :: String,
    stimatedTime :: Int, --Tiempo Estimado en Horas
    startDate :: Date, --Fecha en la que se debe iniciar la tarea
    endDate :: Date, --La tarea debe finalizar en esta fecha o antes
    priority :: Int, --Indica que tan importante es la tarea, si dos tareas comienzan el mismo dia, se debe priorizar la de mayor prioridad
    requiredSkills :: [String],
    requiredResources :: [Resource]
} deriving (Show)