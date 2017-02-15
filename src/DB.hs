{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}


module DB where

import           GHC.Int
import           Control.Monad.IO.Class
import           GHC.Generics (Generic)
import           Database.PostgreSQL.Simple as PGS
import           Data.Pool(Pool, createPool, withResource)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.Double.Conversion.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Models
import           Config

newConn :: DBConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }

fetch :: (FromRow r) => Pool Connection -> Query -> IO [r]
fetch pool sql = withResource pool retrieve
  where retrieve conn = query_ conn sql

fetchArgs :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetchArgs pool args sql = withResource pool retrieve
  where retrieve conn = query conn sql args

execSql :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSql pool args sql = withResource pool ins
  where ins conn = execute conn sql args


route :: (?pool :: Pool Connection) => (Point, Point) -> IO [Route]
route (from, to) = do
  let sql = "SELECT r.distance, ST_AsEncodedPolyline(r.the_geom) FROM tri_route_1((SELECT * from tri_nearest(?, ?)), (SELECT * FROM tri_nearest(?,?))) r;"
  res <- fetchArgs ?pool (lat from, lng from, lat to, lng to) sql :: IO [(Double, T.Text)]
  return $ map (\(dist, polyline) -> Route dist polyline) res

-- Retrieve all segments
segments :: (?pool :: Pool Connection) => IO [Segment]
segments = do
  let sql = "SELECT id, distance, polyline FROM segments;"
  res <- fetch ?pool sql :: IO [(UUID, Double, T.Text)]
  return $ map (\(id, dist, polyline) -> Segment id dist polyline) res

-- Retrieve a segment by ID
findSegment :: (?pool :: Pool Connection) => TL.Text -> IO (Maybe Segment)
findSegment uuid = do
  let sql = "SELECT id, distance, polyline FROM segments WHERE id = ?;"
  res <- fetchArgs ?pool (Only uuid) sql :: IO [(UUID, Double, T.Text)]
  case res of
    [] -> return Nothing
    [(id, dist, polyline)] -> return $ Just $ Segment id dist polyline


-- Save a segment to the db
saveSegment :: (?pool :: Pool Connection) => Segment -> IO Int64
saveSegment (Segment segmentId segmentDistance segmentPolyline) = do
  let sql = "INSERT INTO segments (id, distance, polyline) VALUES (?,?,?);"
  res <- liftIO $ execSql ?pool ([UUID.toText segmentId, toShortest segmentDistance, segmentPolyline]) sql
  return res
