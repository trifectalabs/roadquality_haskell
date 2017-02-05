{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DB where

import           Control.Monad.IO.Class
import           GHC.Generics (Generic)
import           Database.PostgreSQL.Simple as PGS
import           Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Models

data DBConfig = DBConfig {
  dbName :: String,
  dbUser :: String,
  dbPassword :: String
  }
     deriving (Show, Generic)

newConn :: DBConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }

fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
  where retrieve conn = query conn sql args

route :: Pool Connection -> (Point, Point) -> IO [Route]
route pool (from, to) = do
    let sql = "SELECT r.distance, ST_AsEncodedPolyline(r.the_geom) FROM tri_route_1((SELECT * from tri_nearest(?, ?)), (SELECT * FROM tri_nearest(?,?))) r;"
    res <- fetch pool (lat from, lng from, lat to, lng to) sql :: IO [(Double, TL.Text)]
    return $ map (\(dist, polyline) -> Route dist polyline) res
