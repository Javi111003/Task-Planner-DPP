module Model.Resource (Resource(..)) where

data ResourceType = Infinite | Limited deriving (Show, Eq)

data Resource = Resource{
    resourceId :: Int,
    name :: String,
    resourceQty :: Int,
    resourceType :: ResourceType
} deriving (Show, Eq)