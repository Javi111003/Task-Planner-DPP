{-# LANGUAGE DeriveGeneric #-}
module Model.Worker (Worker(..)) where

import Data.Time.Calendar (Day)
import Model.TimeSlot
import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)


data Worker = Worker {
    workerId :: Int,
    workerName :: String,
    availableDays :: [Day],
    maxHoursPerDay :: Int,
    skills :: [String],
    currentSchedule :: [TimeSlot]
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Worker --Autoderivado
instance ToJSON Worker

instance FromJSONKey Worker
instance ToJSONKey Worker