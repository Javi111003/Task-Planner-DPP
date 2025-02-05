{-# LANGUAGE DeriveGeneric #-}
module Model.TimeSlot where

import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)


-- Time slot
-- Representa un espacio de tiempo con inicio y fin
data TimeSlot = TimeSlot
    { 
      --date :: Day,
      startHour :: Int, --9 para 9:00 am
      endHour :: Int  --17 para 5:00 pm
    } deriving (Eq, Ord, Show, Generic)

instance FromJSON TimeSlot --Autoderivado
instance ToJSON TimeSlot

instance FromJSONKey TimeSlot
instance ToJSONKey TimeSlot