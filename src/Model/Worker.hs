module Model.Worker (Worker(..)) where

import Data.Time.Calendar (Day)
import Model.TimeSlot
import Data.Map (Map)

data Worker = Worker {
    workerId :: Int,
    name :: String,
    availableDays :: [Day],
    maxHoursPerDay :: Int,
    skills :: [String],
    currentSchedule :: [TimeSlot]
} deriving (Show, Eq, Ord)