{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import           Web.Scotty
import           Data.Aeson (encode)
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import           Data.Pool(Pool, createPool, withResource)
import           Database.PostgreSQL.Simple
import           Control.Monad.IO.Class

import           Models
import           DB

makeDBConfig :: C.Config -> IO (Maybe DB.DBConfig)
makeDBConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DBConfig <$> name
                    <*> user
                    <*> password

main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDBConfig loadedConf

  case dbConf of
    Nothing -> putStrLn "No database configuration found. Exiting..."
    Just conf -> do
      pool <- createPool (newConn conf) close 1 40 10
      scotty 3000 $ do
        get "/" $ do
          fromLat <- param "from_lat"
          fromLng <- param "from_lng"
          toLat <- param "to_lat"
          toLng <- param "to_lng"

          route <- liftIO $ route pool
            (Point fromLat fromLng, Point toLat toLng)
          segments <- liftIO $ segments pool
          json (route, segments)


