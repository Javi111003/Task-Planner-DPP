module Model.TimeSlot where

import Data.Time.Calendar (Day)

data TimeSlot = TimeSlot
    { date :: Day,
      startHour :: Int, --9 para 9:00 am
      endHour :: Int  --17 para 5:00 pm
    } deriving (Eq, Ord, Show)