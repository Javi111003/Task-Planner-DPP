{-# LANGUAGE DeriveGeneric #-}
module Model.Worker (Worker(..)) where

import Data.Time.Calendar (Day)
import Model.TimeSlot
import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import Data.Set (Set)
import qualified Data.Set as Set

-- Trabajador
-- workerId: Identifica univocamente a cada trabajador
-- workerName: Nombre del trabajador(para mostrar)
-- skills : conjunto de habilidades que posee el trabajador
-- currentSchedule: conjunto de intervalos de tiempo durante el que esta actualmente ocupado
data Worker = Worker {
    workerId :: Int,
    workerName :: String,
    skills :: Set String,
    currentSchedule :: Set TimeSlot,
    availableDays :: Set Day,
    maxHoursPerDay :: Int
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Worker --Autoderivado
instance ToJSON Worker
instance FromJSONKey Worker
instance ToJSONKey Worker