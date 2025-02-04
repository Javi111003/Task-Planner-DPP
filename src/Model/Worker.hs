{-# LANGUAGE DeriveGeneric #-}
module Model.Worker (Worker(..)) where

import Data.Time.Calendar (Day)
import Model.TimeSlot
import Data.Map (Map)
--import GHC.Generics (Generic)
--import Data.Aeson (FromJSON, ToJSON)
--import Data.Aeson.Types (FromJSONKey, ToJSONKey)

-- Trabajador
-- workerId: Identifica univocamente a cada trabajador
-- workerName: Nombre del trabajador(para mostrar)
-- skills : listad de habilidades que posee el trabajador
-- currentSchedule: lista de intervalos de tiempo durante el que esta actualmente ocupado

-- Entidad que representa un trabajador
-- workerId : Identifica univocamente a cada trabajador
-- name : nombre del trabajador(para mostrar)
-- skills: lista de habilidades del trabajador
-- currentSchedule : Lista de intervalos de tiempo en el que esta ocupado durante el dia

data Worker = Worker {
    workerId :: Int,
    workerName :: String,
    skills :: [String],
    currentSchedule :: [TimeSlot]

    --availableDays :: [Day],
    --maxHoursPerDay :: Int,

} deriving (Show, Eq, Ord)

--instance FromJSON Worker --Autoderivado
--instance ToJSON Worker

--instance FromJSONKey Worker
--instance ToJSONKey Worker
