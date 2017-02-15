{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import           GHC.Generics
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import           Network.OAuth.OAuth2
import qualified Data.ByteString.Char8 as C

makeDBConfig :: C.Config -> IO (Maybe DBConfig)
makeDBConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DBConfig <$> name
                    <*> user
                    <*> password

makeOAuthConfig :: C.Config -> IO (Maybe OAuth2)
makeOAuthConfig conf = do
  fbClientId <- C.lookup conf "oauth.facebook.clientId" :: IO (Maybe String)
  fbClientSecret <- C.lookup conf "oauth.facebook.clientSecret" :: IO (Maybe String)
  fbRedirectURI <- C.lookup conf "oauth.facebook.redirectURI" :: IO (Maybe String)
  return $ OAuth2 <$> (C.pack <$> fbClientId)
                  <*> (C.pack <$> fbClientSecret)
                  <*> Just (C.pack "https://www.facebook.com/v2.8/dialog/oauth")
                  <*> Just (C.pack "https://graph.facebook.com/v2.8/oauth/access_token")
                  <*> Just (C.pack <$> fbRedirectURI)

data DBConfig = DBConfig {
  dbName :: String,
  dbUser :: String,
  dbPassword :: String
  } deriving (Show, Generic)

