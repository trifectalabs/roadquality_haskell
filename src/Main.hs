{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell   #-}
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
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import           Data.String.Conversions
import qualified Data.Text.Lazy.Encoding as TL
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import           Data.Maybe
import           Data.Map as Map
import           Network.Wai
import           Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import           Models
import           DB
import           Config
import           OAuth

type SessionStore = Map TL.Text Session

sessionStore :: SessionStore
sessionStore = Map.empty

main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application2.conf"]
  dbConf <- makeDBConfig loadedConf
  oauthConf <- makeOAuthConfig loadedConf

  case (dbConf, oauthConf) of
    (Nothing, Nothing) -> putStrLn "No database or oauth configuration found. Exiting..."
    (Nothing, Just a) -> putStrLn "No database configuration found. Exiting..."
    (Just a, Nothing) -> putStrLn "No oauth configuration found. Exiting..."
    (Just dbConf, Just oauthConf) -> do
      pool <- createPool (newConn dbConf) close 1 40 10
      sessionStore <- newMVar sessionStore
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
          r <- request
          segments <- liftIO $ segments
          authenticatedRequest r (json segments)
          --json segments

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
          redirect $ buildFetchCodeURI oauthConf

        get "/oauth/facebook" $ do
          r <- request
          code <- param "code"
          mgr <- liftIO $ newManager tlsManagerSettings
          let (url, body) = accessTokenUrl oauthConf code
          let uri = appendQueryParam url (transform' [ ("client_id", Just $ oauthClientId oauthConf)
                                                  , ("client_secret", Just $ oauthClientSecret oauthConf)
                                                  , ("state", Just "test")
                                                  ] ++ body)

          req2 <- liftIO $ parseRequest (convertString uri)
          resp <- fmap (parseResponseJSON . handleResponse) (httpLbs req2 mgr)
          case (resp :: OAuth2Result AccessToken) of
            Right token -> do
              user <- liftIO $ userinfo' mgr token
              case (user :: OAuth2Result User) of
                Right user -> do
                  cookie <- parseSessionCookie r
                  session <- liftIO $ createSession user token
                  liftIO $ modifyMVar_ sessionStore $ \s -> do
                    let s' = storeSession s (fromJust cookie) session
                    return s'
                  redirect "/segments"
                Left l -> text $ convertString l
            Left l ->
              text $ convertString l

authenticatedRequest :: Network.Wai.Request -> ActionM () -> ActionM ()
authenticatedRequest r action = do
  cookie <- parseSessionCookie r
  case cookie of
    Just c -> do
      let sessionMaybe = findSession sessionStore c
      case sessionMaybe of
        Just s -> action
        Nothing -> text "cookie not authorized"
    Nothing -> text "no cookie"

buildFetchCodeURI :: OAuth2 -> TL.Text
buildFetchCodeURI oauthConf =
  convertString $ (oauthOAuthorizeEndpoint oauthConf)
                <> "?app_id=" <> (convertString $ oauthClientId oauthConf)
                <> "&redirect_uri=" <> (convertString $ fromJust (oauthCallback oauthConf))
                <> "&scope=" <> "user_about_me,email"

buildFetchAccessTokenURI :: OAuth2 -> TL.Text -> TL.Text
buildFetchAccessTokenURI oauthConf code =
  convertString $ (oauthOAuthorizeEndpoint oauthConf)
                <> "?app_id=" <> (convertString $ oauthClientId oauthConf)
                <> "&app_secret=" <> (convertString $ oauthClientSecret oauthConf)
                <> "&redirect_uri=" <> "http://localhost:3000/oauth/facebook"
                <> "&code=" <> convertString code

userinfo' :: FromJSON User => Manager -> AccessToken -> IO (OAuth2Result User)
userinfo' mgr token = authGetJSON mgr token "https://graph.facebook.com/me?fields=id,name,email"

createSession :: User -> AccessToken -> IO Session
createSession user accessToken = do
  Session <$> nextRandom
          <*> return user

storeSession :: SessionStore -> TL.Text -> Session -> SessionStore
storeSession store cookie session = Map.insert cookie session store

findSession :: SessionStore -> TL.Text -> Maybe Session
findSession store cookie =
  Map.lookup cookie store

parseSessionCookie :: Network.Wai.Request -> ActionM (Maybe TL.Text)
parseSessionCookie r = do
  header "Cookie"


-- Helper method here until requeest fixed https://github.com/freizl/hoauth2/issues/55

