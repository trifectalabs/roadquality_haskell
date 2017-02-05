{-# LANGUAGE DeriveGeneric #-}
module Models
( Route(..) , Point(..) , Segment(..), SegmentForm(..) ) where

import           GHC.Generics
import           Data.Aeson (ToJSON,FromJSON, toJSON, parseJSON)
import           Data.Aeson.Types (Value(String), typeMismatch)
import qualified Data.Text as T
import           Data.UUID (UUID, fromString)
import           Control.Applicative (pure)


instance ToJSON UUID where
  toJSON = String . T.pack . show
instance FromJSON UUID where
  parseJSON json@(String t) =
    let uuidString = T.unpack t
    in case fromString uuidString of
      Just uuid -> pure uuid
      Nothing   -> typeMismatch "UUID" json

instance ToJSON Route
instance FromJSON Route
data Route = Route { routeDistance :: Double, routePolyline :: T.Text } deriving (Eq, Show, Generic)

instance ToJSON Segment
instance FromJSON Segment
data Segment = Segment { segmentId :: UUID, segmentDistance :: Double, segmentPolyline :: T.Text } deriving (Show, Generic)

instance ToJSON SegmentForm
instance FromJSON SegmentForm
data SegmentForm = SegmentForm { segmentFormDistance :: Double, segmentFormPolyline :: T.Text } deriving (Show, Generic)

data Point = Point { lat :: Double, lng :: Double }

