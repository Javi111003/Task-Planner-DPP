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
import Data.List (intersect, union)

-- Validación de habilidades del trabajador
hasRequiredSkills :: Worker -> Task -> Bool
hasRequiredSkills worker task =
  not . null $ skills worker `intersect` requiredSkills task

-- Validación de disponibilidad temporal
isWorkerAvailable :: Worker -> TimeSlot -> SystemState -> Bool
isWorkerAvailable worker@Worker{..} slot SystemState{..} =
  date slot `elem` availableDays &&
  (endHour slot - startHour slot) <= maxHoursPerDay &&
  not (any (overlaps slot) workerSlots)
  where
    workerSlots = Map.findWithDefault [] worker workersSchedule
    overlaps s1 s2 = date s1 == date s2 
                   && startHour s1 < endHour s2 
                   && endHour s1 > startHour s2

-- Validación de recursos
isResourceAvailable :: Resource -> Int -> TimeSlot -> SystemState -> Bool
isResourceAvailable resource qty slot SystemState{..} =
  case resourceType resource of
    Infinite -> True
    Limited ->
      let dayUsage = Map.findWithDefault Map.empty resource resourcesUsage
          slotUsage = Map.findWithDefault Map.empty (date slot) dayUsage
          used = sum $ Map.filterWithKey (\k _ -> overlaps k slot) slotUsage
      in (resourceQty resource - used) >= qty
  where
    overlaps s1 s2 = date s1 == date s2 
                   && startHour s1 < endHour s2 
                   && endHour s1 > startHour s2

-- Validación de grupo de trabajadores
validateWorkerGroup :: [Worker] -> Task -> Bool
validateWorkerGroup workers task =
  all (`hasRequiredSkills` task) workers &&
  (unionAllSkills `containsAll` requiredSkills task)
  where
    unionAllSkills = foldr (\w acc -> skills w `union` acc) [] workers
    containsAll xs ys = all (`elem` xs) ys

-- Validación completa para una tarea
validateAssignment :: Task -> [Worker] -> TimeSlot -> SystemState -> Bool
validateAssignment task workers slot state =
  validateWorkerGroup workers task &&
  all (\w -> isWorkerAvailable w slot state) workers &&
  all (\(r, q) -> isResourceAvailable r q slot state) (requiredResources task)