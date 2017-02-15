{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import           GHC.Generics
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

makeDBConfig :: C.Config -> IO (Maybe DBConfig)
makeDBConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DBConfig <$> name
                    <*> user
                    <*> password

makeOAuthConfig :: C.Config -> IO (Maybe OAuthConfig)
makeOAuthConfig conf = do
  fbClientId <- C.lookup conf "oauth.facebook.clientId" :: IO (Maybe String)
  fbClientSecret <- C.lookup conf "oauth.facebook.clientSecret" :: IO (Maybe String)
  return $ OAuthConfig <$> fbClientId
                       <*> fbClientSecret

data DBConfig = DBConfig {
  dbName :: String,
  dbUser :: String,
  dbPassword :: String
  } deriving (Show, Generic)

data OAuthConfig = OAuthConfig {
  fbClientId :: String,
  fbClientSecret :: String
  } deriving (Show, Generic)
