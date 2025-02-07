{-# LANGUAGE RecordWildCards #-}

module Validation.Constraints where

import Model.Resource
import Model.Task
import Model.Worker
import Model.TimeSlot
import Model.SystemState
import Data.Time.Calendar (Day)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (mapM_)

-- Validación de habilidades del trabajador
hasRequiredSkills :: Worker -> Task -> Bool
hasRequiredSkills worker task = requiredSkills task `Set.isSubsetOf` skills worker

-- Quitar Trabajador Innecesario
isMinCover :: Set Worker -> Task -> Bool
isMinCover workers task = 
    Set.null (Set.filter (\w -> validateWorkerGroup (Set.delete w workers) task) workers)

-- Obtener grupos de trabajadores que pueden hacer una tarea determinada
getValidWorkingTeams :: Task -> Set (Set Worker) -> Set (Set Worker)
getValidWorkingTeams task teams = 
    Set.filter (\team -> validateWorkerGroup team task && isMinCover team task) teams

-- Obtener los horarios en los que se puede realizar una tarea
getValidTimesForTask :: (Task, Set Worker) -> Set TimeSlot
getValidTimesForTask (task, workers) = 
    Set.filter (\t -> isPossibleWorkForAll (Set.toList workers) t) (toDoSlots task)

-- Verifica si todos los trabajadores están disponibles en un horario
isPossibleWorkForAll :: [Worker] -> TimeSlot -> Bool
isPossibleWorkForAll [] _ = True
isPossibleWorkForAll (worker:workers) timeSlot = 
    isWorkerAvailable worker timeSlot && isPossibleWorkForAll workers timeSlot

-- Validación de disponibilidad temporal
isWorkerAvailable :: Worker -> TimeSlot -> Bool
isWorkerAvailable worker slot =
    not (any (overlaps slot) (Set.toList $ currentSchedule worker))
  where
    overlaps s1 s2 = startHour s1 < endHour s2 && endHour s1 > startHour s2

-- Validación de grupo de trabajadores
validateWorkerGroup :: Set Worker -> Task -> Bool
validateWorkerGroup workers task =
    requiredSkills task `Set.isSubsetOf` Set.unions (Set.map skills workers)