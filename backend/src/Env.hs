{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

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
  , sortName    :: IORef Text
  , sorts       :: Vector Text
  , url         :: Text
  , discogs     :: IORef Discogs
  , getList     :: Env -> Text -> IO ( Vector Int )
  , getSort     :: Map Int Album -> Text -> ( Vector Int -> Vector Int )
  }

-- testAlbum :: Album
-- testAlbum = Album 123123
--                   "Test Title"
--                   "Test Artists"
--                   "2021"
--                   "https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg"
--                   "2021-01-01T01:23:01-07:00" 1349997 ( const "xxx" )


initEnv :: IO Env
initEnv = getEnv Nothing Nothing

refreshEnv :: Env -> Text -> Text -> IO Env
refreshEnv env tok un = getEnv (Just env) (Just (Discogs $ DiscogsSession tok un))

-- myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- myFoldr = _

getEnv :: Maybe Env -> Maybe Discogs -> IO Env
getEnv _ Nothing = envFromFiles
getEnv Nothing _ = envFromFiles
getEnv (Just env) (Just discogs') = do
  putTextLn $ "-----------------Updating from " <> show discogs'
  -- we still need the "old" lists map and album map
  oldAlbums <- readIORef $ albums env
  oldLists <- readIORef $ lists env

  -- also save tidal albums
  let (_, tl) = fromMaybe (0, V.empty) $ M.lookup "Tidal" oldLists
      vta :: Vector Album
      vta = V.fromList $ mapMaybe (`M.lookup` oldAlbums) $ V.toList tl
      tidalAlbums = M.fromList $ (\ a -> (albumID a, a)) <$> V.toList vta

  -- refresh Discogs albums info, overwriting changes
  vda <- readAlbums discogs'
  let newAlbums :: Map Int Album
      newAlbums = M.fromList $ (\ a -> (albumID a, a)) <$> V.toList vda
  let allAlbums = newAlbums <> tidalAlbums
  _ <- writeIORef ( albums env ) allAlbums

  -- refresh Discogs folders info
  newFolders <- readFolders discogs' -- readDiscogsFolders
  -- refresh Discogs lists info
  lm <- refreshLists discogs'
  -- refresh folder album ids
  let fm :: Map Text ( Int, Vector Int )
      fm = readFolderAids newFolders allAlbums
  let allLists = lm <> fm
  _ <- M.traverseWithKey ( \ n (i,vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi) ) allLists

  _ <- writeIORef ( lists env ) allLists
  _ <- writeIORef ( listNames env ) ( V.fromList . M.keys $ allLists )
  _ <- writeIORef ( discogs env ) discogs'
  _ <- writeIORef ( sortName env ) "Default"
  return env

envFromFiles :: IO Env
envFromFiles = do
  putTextLn "-------------envFromFiles------------------"

-- return list of Album IDs for List name
--  if list in Env is empty, try to get from provider
  let getList' :: Env -> Text -> IO ( Vector Int )
      getList' env ln = do
        myLists <- readIORef ( lists env )
        -- let getAids :: Text -> IO ( Vector Int )
        --     getAids ln = do
        let (i, aids') = fromMaybe (0, V.empty) (M.lookup ln myLists)
        if V.null aids' then do
          discogs' <- readIORef ( discogs env )
          aids <- readListAids discogs' i
          -- write back modified lists
          _ <- writeIORef ( lists env ) $ M.insert ln (i, aids) myLists
          pure aids
        else pure aids'
        -- getAids ln

  
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
  -- let tidal = Tidal $ TidalFile "data/tall.json"
  let tidal = Tidal $ TidalSession userId sessionId countryCode
  -- let dc = Discogs $ DiscogsSession discogsToken discogsUser
  let dc = Discogs $ DiscogsFile "data/dall.json"

  -- vda/vta :: Vector of Album
  vta <- readAlbums tidal
  vda <- readAlbums dc

  let albums' :: Map Int Album
      albums' = M.fromList $
        (\ a -> (albumID a, a)) <$> V.toList ( vda <> vta )

-- define sort functions and map to names
  let sDef :: Map Int Album -> Vector Int -> Vector Int
      sDef _ l = l
  let sTitle :: Map Int Album -> Vector Int -> Vector Int
      sTitle am aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumTitle) a b ) asi ) where
        asi :: [ ( Int, Maybe Album ) ]
        asi =  map ( \aid -> ( aid, M.lookup aid am ) ) $ V.toList aids
  let sArtist :: Map Int Album -> Vector Int -> Vector Int
      sArtist am aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumArtist) a b ) asi ) where
        asi =  map ( \aid -> ( aid, M.lookup aid am ) ) $ V.toList aids
  let sAdded :: Map Int Album -> Vector Int -> Vector Int
      sAdded am aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumAdded) b a ) asi ) where
        asi =  map ( \aid -> ( aid, M.lookup aid am ) ) $ V.toList aids
  let sfs :: Map Text (Map Int Album -> Vector Int -> Vector Int) -- sort functions
      sfs = M.fromList [ ( "Default", sDef    )
                       , ( "Artist",  sArtist )
                       , ( "Title",   sTitle  )
                       , ( "Added",   sAdded  )
                       ]

      getSort' :: Map Int Album -> Text -> (Vector Int -> Vector Int)
      getSort' am sn = fromMaybe sDef (M.lookup sn sfs) am

      sorts' :: Vector Text
      sorts' = V.fromList $ M.keys sfs
-- read the map of Discogs lists (still empty album ids)
  lm <- readLists dc

-- read the map of Discogs folders
  -- fm' :: Map Text Int
  fm' <- readFolders dc
  let fm :: Map Text ( Int, Vector Int )
      fm = readFolderAids fm' albums'

  let lists' = lm <> fm
  let listNames' = V.fromList ( M.keys lists' )
  _ <- M.traverseWithKey ( \ n (i,vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi) ) lists'
  lnr <- newIORef listNames'
  lr <- newIORef lists'
  dr <- newIORef dc
  ar <- newIORef albums'
  sr <- newIORef "Default"
  return Env { discogs = dr
             , albums = ar, lists = lr
             , listNames = lnr
             , sortName = sr
             , sorts = sorts'
             , url = "/"
             , getList = getList'
             , getSort = getSort'
             }

