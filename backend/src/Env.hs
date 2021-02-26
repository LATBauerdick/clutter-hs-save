{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}


module Env
    ( Env (..)
    , DToken
    -- , testEnv
    , refreshEnv
    , initEnv
    ) where
-- import Prelude hiding ( (++) )

import Data.Maybe ( fromMaybe )
import Data.Vector ( Vector (..) )
import qualified Data.Vector as V ( fromList
                                  , toList
                                  , map
                                  , filter
                                  , singleton
                                  , null
                                  , empty
                                  )
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

-- import qualified Data.Foldable as F ( traverse_  )

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Provider ( Tidal (..)
                , TidalInfo (..)
                , Discogs (..)
                , DiscogsInfo (..)
                , Album (..)
                , DToken (..)
                , readAlbums
                , readLists
                , readListAids
                , readFolders
                , refreshLists
                )

data Env
  = Env
  { albums      :: M.Map Int Album
  , listNames   :: Vector Text
  , lists       :: M.Map Text (Int, Vector Int)
  , getList     :: Text -> IO ( Vector Int )
  , sorts       :: Vector Text
  , getSort     :: Text -> ( Vector Int -> Vector Int )
  , url         :: Text
  , discogs     :: Discogs
  , token       :: DToken
  }

refreshEnv :: Env -> Text -> Text -> IO Env
refreshEnv env tok un = initEnv (Just env) (Just (DToken tok un))

-- testEnv :: Env
-- testEnv = Env { albums = M.singleton 1 testAlbum, url = "/", listNames = V.empty }

testAlbum :: Album
testAlbum = Album 123123
                  "Test Title"
                  "Test Artists"
                  "2021"
                  "https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg"
                  "2021-01-01T01:23:01-07:00" 1349997 ( const "xxx" )

initEnv :: Maybe Env -> Maybe DToken -> IO Env
initEnv _ Nothing = envFromFiles
initEnv Nothing _ = envFromFiles -- prelim: always read from files
initEnv (Just e) (Just dtok) = do
  putStrLn $ "token " ++ show dtok
  refreshLists dtok
  return e -- do nothing yet

envFromFiles :: IO Env
envFromFiles = do

-- define sort functions and map to names
  let asi :: Env -> Vector Int -> [ ( Int, Maybe Album ) ]
      asi env aids =  map ( \aid -> ( aid, M.lookup aid ( albums env ) ) ) $ V.toList aids
  let sDef :: Env -> Vector Int -> Vector Int
      sDef _ l = l
  let sAdded :: Env -> Vector Int -> Vector Int
      sAdded env aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumAdded) b a ) ( asi env aids ))
  let sArtist :: Env -> Vector Int -> Vector Int
      sArtist env aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumArtist) a b ) ( asi env aids))
  let sTitle :: Env -> Vector Int -> Vector Int
      sTitle env aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumTitle) a b ) ( asi env aids))
  let sfs :: M.Map Text (Env -> Vector Int -> Vector Int) -- sort functions
      sfs = M.fromList [ ( "Default", sDef    )
                       , ( "Artist",  sArtist )
                       , ( "Title",   sTitle  )
                       , ( "Added",   sAdded  )
                       ]

      getSort :: Env -> Text -> (Vector Int -> Vector Int)
      getSort env t = fromMaybe sDef (M.lookup t sfs) env

      sorts :: Vector Text
      sorts = V.fromList $ M.keys sfs

-- return list of albumIDs, sorted for "Added"
  let sls :: Env -> Vector Album -> Vector Int
      sls env as = sAdded env ( albumID <$> as )
-- return list of Album IDs for List name
--  if list in Env is empty, try to get from provider
  let getList :: Env -> Text -> IO ( Vector Int )
      getList env ln = do
        let getAids :: Text -> IO ( Vector Int )
            getAids ln = do
              let (i, aids') = fromMaybe (0, V.empty) (M.lookup ln (lists env))
              if V.null aids' then do
                readListAids ( discogs env ) i
                else pure aids'
              -- V.singleton 17183404 -- do readListAids etc
        -- let sort = getSort env "Default"
        getAids ln

      listNames :: Env -> Vector Text
      listNames env = V.fromList $ M.keys (lists env)
--
-- get Map of all albums from Providers:
-- retrieve database from files
--
  -- vda/vta :: Vector Album
  -- vta <- readAlbums $ Tidal $ TidalFile "data/tall.json"
  let discogs = Discogs $ DiscogsFile "data/dall.json"

-- retrieve database from Web
  t <- readFile "data/tok.dat" -- for debug, get from file with authentication data
  let userId = read ( words t !! 2 ) :: Int
      sessionId = T.pack $ words t !! 3
      countryCode = T.pack $ words t !! 4
      discogsToken = T.pack $ head . words $ t
      discogsUser = T.pack $ words t !! 1

  let discogs = Discogs $ DiscogsSession discogsToken discogsUser
  vta <- readAlbums $ Tidal $ TidalSession userId sessionId countryCode
  vda <- readAlbums discogs

-- read the map of Discogs lists (still empty album ids)
  lm <- readLists discogs

-- read the map of Discogs folders
  -- fm' :: M.Map Text Int
  let filtFolder :: Int -> Vector Int
      filtFolder fid =
          V.map fst
          $ V.filter (\ (a,f) -> f==fid)
          $ V.map (\a -> (albumID a, albumFolder a)) vda
  fm' <- readFolders discogs
  let fm :: M.Map Text ( Int, Vector Int )
      getFolder :: Text -> Int -> (Int, Vector Int)
      getFolder n i = (i, filtFolder i) --  V.empty
      fm = M.mapWithKey getFolder fm'

  let albums :: M.Map Int Album
      albums = M.fromList $
        (\ a -> (albumID a, a)) <$> V.toList ( vda <> vta )

-- construct the map to retrieve lists/folders of Albums by name
  let am :: M.Map Text (Int, Vector Int)
      am = lm <> fm
-- add the Tidal, Discogs, and the All lists "by hand"
-- and sort them ("Default") for Date Added

      om :: M.Map Text (Int, Vector Int)
      om = M.fromList [ ("Discogs", (2, sls env vda) )
                      , ("Tidal",   (3, sls env vta) )
                      , ("All",     (4, sls env (vda <> vta)) )
                      ] where env = Env { albums = albums }
  let lists :: M.Map Text ( Int, Vector Int )
      lists = M.union om am

  let this = Env { discogs = discogs, albums = albums, lists = lists }
  return this { listNames = listNames this
              , getList = getList this
              , getSort = getSort this
              , sorts = sorts
              , url = "/"
              , token = undefined
              }

