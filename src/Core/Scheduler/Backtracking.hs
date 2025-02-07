{-# LANGUAGE RecordWildCards #-}

module Core.Scheduler.Backtracking where

import Model.Task (Task(..))
import Model.Worker (Worker(..))
import Model.TimeSlot (TimeSlot(..))
import Validation.Constraints (getValidWorkingTeams, getValidTimesForTask)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Encuentra todas las posibles soluciones y selecciona la mejor
solve :: Set Task -> Set Worker -> IO ([(Task, Set Worker, TimeSlot)], Set Task)
solve tasks workers = do 
    results <- find_all tasks workers
    let bestSolution = getBest results
    let assignedTasks = Set.fromList $ map (\(task, _, _) -> task) bestSolution
    let unassignedTasks = Set.difference tasks assignedTasks
    return (bestSolution, unassignedTasks)

getBest :: [[(Task, Set Worker, TimeSlot)]] -> [(Task, Set Worker, TimeSlot)]
getBest [] = []
getBest lists = maximumBy (comparing length) lists

-- Encuentra todas las posibles soluciones mediante la técnica de Backtracking
find_all :: Set Task -> Set Worker -> IO [[(Task, Set Worker, TimeSlot)]]
find_all tasks workers
    | Set.null tasks = return [[]]
    | otherwise = do
        let (t, ts) = (Set.findMin tasks, Set.delete (Set.findMin tasks) tasks)
        results <- assignTask t ts workers
        restResults <- find_all ts workers
        return (results ++ restResults)

assignTask :: Task -> Set Task -> Set Worker -> IO [[(Task, Set Worker, TimeSlot)]]
assignTask task tasks workers = do
    let workerSubsets = powerSet workers
    let validTeams = getValidWorkingTeams task workerSubsets
    results <- mapM assignTeams (Set.toList validTeams)
    return (concat results)
  where
    assignTeams team = do
      let validTimes = Set.toList $ getValidTimesForTask (task, team)
      assignments <- mapM (\assTime -> do
          let updatedWorkers = updateWorkers workers team assTime
          assignRest <- find_all tasks updatedWorkers
          let currentAssignment = [(task, team, assTime)]
          return $ if null assignRest
                  then [currentAssignment]
                  else map (currentAssignment ++) assignRest) validTimes
      return (concat assignments)

updateWorkers :: Set Worker -> Set Worker -> TimeSlot -> Set Worker
updateWorkers allWorkers workersToUpdate newTimeSlot = 
    Set.map updateWorker allWorkers
  where
    updateWorker worker
      | Set.member worker workersToUpdate = worker { currentSchedule = Set.insert newTimeSlot (currentSchedule worker) }
      | otherwise = worker

powerSet :: Ord a => Set a -> Set (Set a)
powerSet = Set.foldr (\x acc -> acc `Set.union` Set.map (Set.insert x) acc) (Set.singleton Set.empty)
