{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}

import           Web.Scotty
import           Data.Aeson (encode, decode)
import           Data.Aeson (FromJSON, ToJSON, parseJSON)
import           GHC.Generics
import           Data.Pool(Pool, createPool, withResource)
import           Database.PostgreSQL.Simple
import           Control.Monad.IO.Class
import qualified Data.Configurator as C
import           Network.HTTP.Types.Status
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (UUID, toString)
import qualified Data.UUID as UUID
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Models
import           DB
import           Config


main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDBConfig loadedConf
  oauthConf <- makeOAuthConfig loadedConf

  case dbConf of
    Nothing -> putStrLn "No database configuration found. Exiting..."
    Just conf -> do
      pool <- createPool (newConn conf) close 1 40 10
      let ?pool = pool
      scotty 3000 $ do

        get "/route" $ do
          fromLat <- param "from_lat"
          fromLng <- param "from_lng"
          toLat <- param "to_lat"
          toLng <- param "to_lng"

          route <- liftIO $ route
            (Point fromLat fromLng, Point toLat toLng)
          json route

        get "/segments" $ do
          segments <- liftIO $ segments
          json segments

        get "/segments/:uuid" $ do
          uuid <- param "uuid"
          segment <- liftIO $ findSegment uuid
          case segment of
            Just s -> json s
            Nothing -> status notFound404

        put "/segments" $ do
          b <- body
          let segForm  = (decode b :: Maybe SegmentForm)
          case segForm of
            Just segment -> do
              uuid <- liftIO nextRandom
              let seg = Segment uuid (segmentFormDistance segment) (segmentFormPolyline segment)
              segm <- liftIO $ saveSegment seg
              json seg
            Nothing      -> status badRequest400

