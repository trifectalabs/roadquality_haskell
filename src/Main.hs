{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Data.String.Conversions
import qualified Data.Text.Lazy.Encoding as TL
import           Network.OAuth.OAuth2
import           Data.Maybe

import           Models
import           DB
import           Config
import           OAuth


main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDBConfig loadedConf
  oauthConf <- makeOAuthConfig loadedConf

  case (dbConf, oauthConf) of
    (Nothing, Nothing) -> putStrLn "No database or oauth configuration found. Exiting..."
    (Nothing, Just a) -> putStrLn "No database configuration found. Exiting..."
    (Just a, Nothing) -> putStrLn "No oauth configuration found. Exiting..."
    (Just dbConf, Just oauthConf) -> do
      pool <- createPool (newConn dbConf) close 1 40 10
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

        get "/oauth" $ do
          redirect $ convertString $ (oauthOAuthorizeEndpoint oauthConf)
            <> "?app_id=" <> (convertString $ oauthClientId oauthConf)
            <> "&redirect_uri=" <> (convertString $ fromJust (oauthCallback oauthConf))

        get "/oauth/facebook" $ do
          code <- param "code"
          text code
