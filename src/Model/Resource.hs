{-# LANGUAGE DeriveGeneric #-}
module Model.Resource (Resource(..), ResourceType(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)

data ResourceType = Infinite | Limited deriving (Show, Eq, Ord, Generic)

instance FromJSON ResourceType
instance ToJSON ResourceType

data Resource = Resource{
    resourceId :: Int,
    name :: String,
    resourceQty :: Int,
    resourceType :: ResourceType
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Resource 
instance ToJSON Resource

instance FromJSONKey Resource
instance ToJSONKey Resource