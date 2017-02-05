{-# LANGUAGE DeriveGeneric #-}
module Models
( Route(..), Point(..) ) where

import           GHC.Generics
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text.Lazy

instance ToJSON Route
instance FromJSON Route
data Route = Route { distance :: Double, polyline :: Text } deriving (Eq, Show, Generic)

data Point = Point { lat :: Double, lng :: Double }

