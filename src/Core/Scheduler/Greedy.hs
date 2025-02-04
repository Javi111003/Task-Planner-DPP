{-# LANGUAGE RecordWildCards #-}

module Core.Scheduler.Greedy where

import Model.Resource 
import Model.Task
import Model.TimeSlot
import Model.Worker
import Model.SystemState
import Validation.Constraints
import Data.List (sortOn)
import Data.Time.Calendar (Day, addDays)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)


-- Vamos a cambiar la estrategia de optimizacion el nuevo greedy usara el algoritmo de backtraking para maximizar la cantidad de tareas realizadas dia por dia

-- Priorización de tareas
-- prioritizeTasks :: [Task] -> [Task]
-- prioritizeTasks = sortOn deadline

-- Búsqueda de slots disponibles
--findAvailableSlot :: Task -> [Worker] -> SystemState -> Maybe TimeSlot
--findAvailableSlot task workers state =
--  listToMaybe validSlots
--  where
--    validSlots = 
--      [ slot
--      | days <- [deadline task,(addDays (-1) (deadline task)) ..]  -- Buscar desde el deadline hacia atrás
--     , start <- [9 .. 17 - estimatedTime task]             -- Horario laboral de 9 a 17
--      , let slot = TimeSlot days start (start + estimatedTime task)
--      , validateAssignment task workers slot state
--      ]

-- Actualización del estado del sistema
--updateSystemState :: Task -> [Worker] -> TimeSlot -> SystemState -> SystemState
--updateSystemState task workers slot state@SystemState{..} =
--  state
--    { workersSchedule = updateWorkerSchedule
--    , resourcesUsage = updateResourceUsage
--    , assignedTasks = Map.insert task (slot, workers, requiredResources task) assignedTasks
--   }
--  where
--    updateWorkerSchedule = foldr (Map.alter insertSlot) workersSchedule workers
--    insertSlot Nothing = Just [slot]
--    insertSlot (Just slots) = Just (slot : slots)
    
--    updateResourceUsage = foldr (\(r, q) acc ->
--      Map.alter (updateResourceDay q slot) r acc) resourcesUsage (requiredResources task)
    
--    updateResourceDay q slot Nothing = 
--      Just $ Map.singleton (date slot) (Map.singleton slot q)
--    updateResourceDay q slot (Just dayMap) =
--      Just $ Map.alter (updateSlot q slot) (date slot) dayMap
    
--    updateSlot q slot Nothing = Just (Map.singleton slot q)
--    updateSlot q slot (Just slotMap) = Just (Map.insertWith (+) slot q slotMap)

-- Algoritmo principal de scheduling
--scheduleTasks :: [Task] -> [Worker] -> SystemState -> SystemState
--scheduleTasks tasks workers initialState =
--  foldl processTask initialState (prioritizeTasks tasks)
--  where
--    processTask state task = case findAvailableSlot task workers state of
--      Just slot -> updateSystemState task workers slot state
--     Nothing -> state { unassignedTasks = task : unassignedTasks state }