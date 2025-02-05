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
import Control.Monad (mapM_)

-- Validación de habilidades del trabajador
hasRequiredSkills :: Worker -> Task -> Bool
hasRequiredSkills worker task = all (`elem` skills worker) (requiredSkills task)

--Quitar Trabajador Innecesario
isMinCover :: [Worker] -> Task -> Bool
isMinCover workers task = null [w | w <- workers, validateWorkerGroup (remove workers w) task]

--Obtener que grupos de Trabajadores pueden hacer una tarea determinada
getValidWorkingTeams :: Task -> [[Worker]] -> [[Worker]]
getValidWorkingTeams task teams = 
    [team | team <- teams, validateWorkerGroup team task , isMinCover team task]
    where
        -- Impresión de depuración
        _ = mapM_ (\team -> 
            let isValid = validateWorkerGroup team task && isMinCover team task
            in putStrLn $ "Evaluating team: " ++ show (map workerName team) ++ 
                          " | Valid: " ++ show isValid) teams

--Obtener los horarios en los que se puede realizar una tarea
getValidTimesForTask :: (Task,[Worker]) -> TimeSlot -> [TimeSlot]
getValidTimesForTask (task, workers) lot  = let ts = [TimeSlot a b | a <- [(startHour lot) .. (endHour lot)], b <- [(startHour lot) .. (endHour lot)], a <= b, b - a == estimatedTime task]
                                        in [ t| t <- ts, isPossibleWorkForAll workers t ]

isPossibleWorkForAll :: [Worker] -> TimeSlot -> Bool
isPossibleWorkForAll [] _ = True
isPossibleWorkForAll (worker:workers) timeSlot = isWorkerAvailable worker timeSlot && isPossibleWorkForAll workers timeSlot

-- Validación de disponibilidad temporal
isWorkerAvailable :: Worker -> TimeSlot -> Bool
isWorkerAvailable worker slot =
  not (any (overlaps slot) (currentSchedule worker))
  where overlaps s1 s2 = startHour s1 <= endHour s2 && endHour s1 >= startHour s2

-- Validación de recursos
--isResourceAvailable :: Resource -> Int -> TimeSlot -> SystemState -> Bool
--isResourceAvailable resource qty slot SystemState{..} =
--  case resourceType resource of
--    Infinite -> True
--    Limited ->
--      let dayUsage = Map.findWithDefault Map.empty resource resourcesUsage
--          slotUsage = Map.findWithDefault Map.empty (date slot) dayUsage
--          used = sum $ Map.filterWithKey (\k _ -> overlaps k slot) slotUsage
--      in (resourceQty resource - used) >= qty
--  where
--    overlaps s1 s2 = date s1 == date s2
--                   && startHour s1 <= endHour s2
--                   && endHour s1 => startHour s2

-- Validación de grupo de trabajadores
validateWorkerGroup :: [Worker] -> Task -> Bool
validateWorkerGroup workers task =
  all (`elem` concatMap skills workers) (requiredSkills task)

-- Validación completa para una tarea
--validateAssignment :: Task -> [Worker] -> TimeSlot -> SystemState -> Bool
--validateAssignment task workers slot state =
--  validateWorkerGroup workers task &&
--  all (\w -> isWorkerAvailable w slot state) workers &&
--  all (\(r, q) -> isResourceAvailable r q slot state) (requiredResources task)

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) y = if x == y then xs else x: remove xs y