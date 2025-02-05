module Core.Scheduler.Backtracking where

import Model.Task
import Model.Worker
import Model.TimeSlot
import Validation.Constraints (getValidWorkingTeams, getValidTimesForTask)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Encuentra todas las posibles soluciones y seleciona la mejor
solve :: [Task] -> [Worker] -> TimeSlot -> IO [(Task, [Worker], TimeSlot)]
solve tasks workers t = do 
    results <- find_all tasks workers t  -- Captura el resultado de find_all
    return (getBest results)  -- Usa getBest en los resultados

getBest :: [[(Task, [Worker], TimeSlot)]] -> [(Task, [Worker], TimeSlot)]
getBest [] = []  -- Manejar el caso vacío
getBest lists = maximumBy (comparing length) lists  -- Retornar la lista con mayor longitud

-- Encuentra todas las posibles soluciones mediante la tecnica de Backtracking lo cojo o no lo cojo
find_all :: [Task] -> [Worker] -> TimeSlot -> IO [[(Task, [Worker], TimeSlot)]]
find_all [] _  _ = return []  -- Retorna una lista vacía en IO
find_all (t : ts) ws time = do
    results <- assignTask (t:ts) ws time
    restResults <- find_all ts ws time  -- Llama recursivamente
    combinedResults <- return (results ++ restResults)  -- Combina los resultados
    return combinedResults  -- Retorna los resultados combinados

-- Halla todas las posibles combinaciones tras asignar la tarea a cada uno de sus posibles equipos de trabajo y horario
assignTask :: [Task] -> [Worker] -> TimeSlot -> IO [[(Task, [Worker], TimeSlot)]]
assignTask (t : ts) workers time = do
    let workerSubsets = powerSet workers
    let validTeams = getValidWorkingTeams t workerSubsets

    results <- mapM assignTeams validTeams
    let finalResults = concat results
    return finalResults
  where
    assignTeams w = do
      let validTimes = getValidTimesForTask (t, w) time
      assignments <- mapM (\assTime -> do
        let updatedWorkers = updateWorkers workers w assTime
        assignRest <- find_all ts updatedWorkers time
        let currentAssignment = [(t, w, assTime)]
        return $ if null assignRest
                then [currentAssignment]
                else map (currentAssignment ++) assignRest) validTimes
      return (concat assignments)

updateWorkers :: [Worker] -> [Worker] -> TimeSlot -> [Worker]
updateWorkers allWorkers workersToUpdate newTimeSlot = 
    map updateWorker allWorkers
  where
    updateWorker worker
      | worker `elem` workersToUpdate = worker { currentSchedule = newTimeSlot : currentSchedule worker }
      | otherwise = worker

powerSet :: [a] -> [[a]]
powerSet [] = [[]]  -- El conjunto potencia de una lista vacía es una lista que contiene el conjunto vacío
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)
