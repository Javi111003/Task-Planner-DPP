module Model.Resource (Resource(..), ResourceType(..)) where

data ResourceType = Infinite | Limited deriving (Show, Eq, Ord)

data Resource = Resource{
    resourceId :: Int,
    name :: String,
    resourceQty :: Int,
    resourceType :: ResourceType
} deriving (Show, Eq, Ord)