{-# LANGUAGE DeriveGeneric #-}
module Model.TimeSlot where

import Data.Time.Calendar (Day)
--import GHC.Generics (Generic)
--import Data.Aeson (FromJSON, ToJSON)
--import Data.Aeson.Types (FromJSONKey, ToJSONKey)


-- Time slot
-- Representa un espacio de tiempo con inicio y fin
data TimeSlot = TimeSlot
    { 
      --date :: Day,
      startHour :: Int, 
      endHour :: Int  
    
    } deriving (Eq, Ord, Show)

--instance FromJSON TimeSlot --Autoderivado
--instance ToJSON TimeSlot

--instance FromJSONKey TimeSlot
--instance ToJSONKey TimeSlot