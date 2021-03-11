{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}


module Env
    ( Env (..)
    -- , DToken
    -- , testEnv
    , refreshEnv
    , initEnv
    ) where
import Relude

import Data.Vector ( Vector )
import qualified Data.Vector as V ( fromList
                                  , toList
                                  , null
                                  , empty
                                  )
-- import Data.List (sortBy)
-- import Data.Ord (comparing)
import qualified Data.Map.Strict as M

import Provider ( Tidal (..)
                , TidalInfo (..)
                , Discogs (..)
                , DiscogsInfo (..)
                , Album (..)
                , readAlbums
                , readLists
                , readListAids
                , readFolders
                , readFolderAids
                , refreshLists
                )

data Env
  = Env
  { albums      :: IORef ( Map Int Album )
  , listNames   :: IORef ( Vector Text )
  , lists       :: IORef ( Map Text (Int, Vector Int) )
  , sorts       :: Vector Text
  , url         :: Text
  , discogs     :: IORef Discogs
  -- , token       :: DToken
  , getList     :: Env -> Text -> IO ( Vector Int )
  , getSort     :: Env -> Text -> ( Vector Int -> Vector Int )
  }

testAlbum :: Album
testAlbum = Album 123123
                  "Test Title"
                  "Test Artists"
                  "2021"
                  "https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg"
                  "2021-01-01T01:23:01-07:00" 1349997 ( const "xxx" )


initEnv :: IO Env
initEnv = getEnv Nothing Nothing

refreshEnv :: Env -> Text -> Text -> IO Env
refreshEnv env tok un = getEnv (Just env) (Just (Discogs $ DiscogsSession tok un))

getEnv :: Maybe Env -> Maybe Discogs -> IO Env
getEnv _ Nothing = envFromFiles
getEnv Nothing _ = envFromFiles -- prelim: always read from files
getEnv (Just env) (Just discogs') = do
  putTextLn $ "-----------------Updating from " <> show discogs'
  -- we still need the "old" lists map and album map
  oldAlbums <- readIORef $ albums env
  oldLists <- readIORef $ lists env
  -- also save tidal albums
  let (_, tl) = fromMaybe (0, V.empty) $ M.lookup "Tidal" oldLists

  -- refresh Discogs albums info, overwriting changes
  vda <- readAlbums discogs'
  let newAlbums :: Map Int Album
      newAlbums = M.fromList $ (\ a -> (albumID a, a)) <$> V.toList vda
  let allAlbums = newAlbums <> oldAlbums
  _ <- writeIORef ( albums env ) allAlbums

  -- refresh Discogs folders info
  newFolders <- readFolders discogs' -- readDiscogsFolders
  -- refresh Discogs lists info
  lm <- refreshLists discogs'
  -- refresh folder album ids
  let fm :: Map Text ( Int, Vector Int )
      fm = readFolderAids newFolders allAlbums
  let allLists = lm <> fm
  -- _ <- M.traverseWithKey ( \ n (i,vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi) ) allLists

  _ <- writeIORef ( lists env ) allLists
  _ <- writeIORef ( listNames env ) ( V.fromList . M.keys $ allLists )
  _ <- writeIORef ( discogs env ) discogs'
  return env

envFromFiles :: IO Env
envFromFiles = do
  putTextLn "-------------envFromFimes------------------"
-- define sort functions and map to names
  -- let asi :: Env -> Vector Int -> [ ( Int, Maybe Album ) ]
  --     asi env aids =  map ( \aid -> ( aid, M.lookup aid ( albums env ) ) ) $ V.toList aids
  let sDef :: Env -> Vector Int -> Vector Int
      sDef _ l = l
  -- let sAdded :: Env -> Vector Int -> Vector Int
  --     sAdded env aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumAdded) b a ) ( asi env aids ))
  -- let sArtist :: Env -> Vector Int -> Vector Int
  --     sArtist env aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumArtist) a b ) ( asi env aids))
  -- let sTitle :: Env -> Vector Int -> Vector Int
  --     sTitle env aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumTitle) a b ) ( asi env aids))
  let sfs :: Map Text (Env -> Vector Int -> Vector Int) -- sort functions
      sfs = M.fromList [ ( "Default", sDef    )
                       -- , ( "Artist",  sArtist )
                       -- , ( "Title",   sTitle  )
                       -- , ( "Added",   sAdded  )
                       ]

      getSort :: Env -> Text -> (Vector Int -> Vector Int)
      getSort env t = fromMaybe sDef (M.lookup t sfs) env

      sorts :: Vector Text
      sorts = V.fromList $ M.keys sfs

-- return list of Album IDs for List name
--  if list in Env is empty, try to get from provider
  let getList :: Env -> Text -> IO ( Vector Int )
      getList env ln = do
        myLists <- readIORef ( lists env )
        let getAids :: Text -> IO ( Vector Int )
            getAids ln = do
              let (i, aids') = fromMaybe (0, V.empty) (M.lookup ln myLists)
              if V.null aids' then do
                  discogs <- readIORef ( discogs env )
                  aids <- readListAids discogs i
                -- write back modified lists
                  _ <- writeIORef ( lists env ) $ M.insert ln (i, aids) myLists
                  pure aids
                else pure aids'
        -- let sort = getSort env "Default"
        getAids ln

  
--
-- get Map of all albums from Providers:
-- retrieve database from files
--
-- debug: get web credentials etc
  t <- readFileText "data/tok.dat" -- for debug, get from file with authentication data
  let [t0, t1, t2, t3, t4] = words t
      countryCode = t4
      sessionId = t3
      userId = fromMaybe 0 $ readMaybe ( toString t2 ) :: Int
      discogsToken = t0
      discogsUser = t1
  let tidal = Tidal $ TidalFile "data/tall.json"
  let tidal = Tidal $ TidalSession userId sessionId countryCode
  let discogs = Discogs $ DiscogsFile "data/dall.json"
  -- let discogs = Discogs $ DiscogsSession discogsToken discogsUser

  -- vda/vta :: Vector of Album
  vta <- readAlbums tidal
  vda <- readAlbums discogs

  let albums :: Map Int Album
      albums = M.fromList $
        (\ a -> (albumID a, a)) <$> V.toList ( vda <> vta )

-- read the map of Discogs lists (still empty album ids)
  lm <- readLists discogs

-- read the map of Discogs folders
  -- fm' :: Map Text Int
  fm' <- readFolders discogs
  let fm :: Map Text ( Int, Vector Int )
      fm = readFolderAids fm' albums

  let lists = lm <> fm
  let listNames = V.fromList ( M.keys lists )
  M.traverseWithKey ( \ n (i,vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi) ) lists
  lnr <- newIORef listNames
  lr <- newIORef lists
  dr <- newIORef discogs
  ar <- newIORef albums
  return Env { discogs = dr
             , albums = ar, lists = lr
             , listNames = lnr
             , sorts = sorts
             , url = "/"
             , getList = getList
             , getSort = getSort
             }

-- return list of albumIDs, sorted for "Added"
-- define sort functions and map to names
-- sls :: Map Int Album -> Vector Int -> Vector Int
-- sls am ais = sAdded am ais where
sAdded :: Map Int Album -> Vector Int -> Vector Int
sAdded am aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumAdded) b a ) ( asi am aids )) where
  asi :: Map Int Album -> Vector Int -> [ ( Int, Maybe Album ) ]
  asi am aids =  map ( \aid -> ( aid, M.lookup aid am ) ) $ V.toList aids
