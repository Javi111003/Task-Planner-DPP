module Resource (Resource(..)) where
import Date

data Resource = Resource{
    resourceId :: Int,
    name :: String,
    disponibilityDays :: [Date]
} deriving (Show)