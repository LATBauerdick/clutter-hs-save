{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}


module Env
    ( refreshEnv
    , initEnv
    ) where
import Relude

import Data.Vector ( Vector )
import qualified Data.Vector as V ( fromList
                                  , toList
                                  , reverse
                                  , null
                                  , empty
                                  )
-- import Data.List (sortBy)
-- import Data.Ord (comparing)
import qualified Data.Map.Strict as M

import Types ( Album (..), TidalInfo (..), SortOrder (..), Env (..)
             , Tidal (..)
             , Discogs (..), DiscogsInfo (..)
             )

import Provider ( readAlbums
                , readLists
                , readListAids
                , readFolders
                , readFolderAids
                , rereadLists
                )

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

-- initialize env: if not exest yet, get from file, otherwise from Discogs
getEnv :: Maybe Env -> Maybe Discogs -> IO Env
getEnv _ Nothing = envFromFiles
getEnv Nothing _ = envFromFiles
getEnv (Just env) (Just discogs') = do
  putTextLn $ "-----------------Updating from " <> show discogs'
  -- we still need the "old" lists map and album map
  oldAlbums <- readIORef $ albumsR env
  oldLists <- readIORef $ listsR env
  -- oldLocs <- readIORef $ locsR env

  -- also save tidal albums
  let (_, tl) = fromMaybe (0, V.empty) $ M.lookup "Tidal" oldLists
      vta :: Vector Album
      vta = V.fromList $ mapMaybe (`M.lookup` oldAlbums) $ V.toList tl
      tidalAlbums = M.fromList $ (\ a -> (albumID a, a)) <$> V.toList vta

  -- reread Discogs albums info, overwriting changes
  vda <- readAlbums discogs'
  let newAlbums :: Map Int Album
      newAlbums = M.fromList $ (\ a -> (albumID a, a)) <$> V.toList vda
  let allAlbums = newAlbums <> tidalAlbums
  _ <- writeIORef ( albumsR env ) allAlbums

  -- reread Discogs folders info
  newFolders <- readFolders discogs' -- readDiscogsFolders
  -- reread Discogs lists info
  lm <- rereadLists discogs'
  -- reread folder album ids
  let fm :: Map Text ( Int, Vector Int )
      fm = readFolderAids newFolders allAlbums
  let allLists = lm <> fm
  _ <- M.traverseWithKey ( \ n (i,vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi) ) allLists

  let allLocs = updateLocations lm

  _ <- writeIORef ( listsR env ) allLists
  _ <- writeIORef ( locsR env ) allLocs
  _ <- writeIORef ( listNamesR env ) ( V.fromList . M.keys $ allLists )
  _ <- writeIORef ( discogsR env ) discogs'
  _ <- writeIORef ( sortNameR env ) "Default"
  _ <- writeIORef ( sortOrderR env ) Asc
  return env

updateLocations :: Map Text (Int, Vector Int) -> Map Int (Text, Int)
updateLocations = M.fromList . concatMap xxx . filter p . M.toList where
  xxx :: (Text, (Int, Vector Int)) -> [(Int, (Text, Int))]
  xxx (ln, (_, aids)) = zipWith (\ aid idx -> (aid, (ln, idx))) [1..] (V.toList aids)
  p :: (Text, (Int, Vector Int)) -> Bool
  p (ln, _) = pLoc ln
  pLoc :: Text -> Bool  -- lists with location info
  pLoc n = case viaNonEmpty head . words $ n of
                  Just "Cube"   -> True
                  Just "Shelf"  -> True
                  _             -> False

envFromFiles :: IO Env
envFromFiles = do
  putTextLn "-------------envFromFiles------------------"

-- define the function for (env getList) :: Env -> Text -> IO ( Vector Int )
-- that will return list of Album IDs for List name
--  if list in Env is empty, try to get from provider
  let getList' :: Env -> Text -> IO ( Vector Int )
      getList' env ln = do
        lists' <- readIORef ( listsR env )
        let (lid, aids') = fromMaybe (0, V.empty) (M.lookup ln lists')
        if V.null aids' then do
          discogs' <- readIORef ( discogsR env )
          aids <- readListAids discogs' lid
          -- update location info in albums
          --   go through this list and update location in albums
          -- am <- liftIO ( readIORef (albumsR env) )
          -- am' <- updateLocations lists ln am aids -- not yet implemented
          -- _ <- writeIORef (albumsR env) am'
          -- write back modified lists
          _ <- writeIORef ( listsR env ) $ M.insert ln (lid, aids) lists'
          pure aids
        else pure aids'

  
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
  -- let tidal = Tidal $ TidalSession userId sessionId countryCode
  -- let dc = Discogs $ DiscogsSession discogsToken discogsUser
  let dc = Discogs $ DiscogsFile "data/dall.json"

  -- vda/vta :: Vector of Album
  vta <- readAlbums tidal
  vda <- readAlbums dc

  let albums' :: Map Int Album
      albums' = M.fromList $
        (\ a -> (albumID a, a)) <$> V.toList ( vda <> vta )

-- define sort functions and map to names
  let sDef :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sDef _ so l = case so of
                      Asc -> l
                      _ -> V.reverse l
  let sortAsi :: Map Int Album -> Vector Int -> [ ( Int, Maybe Album ) ]
      sortAsi am =  map ( \aid -> ( aid, M.lookup aid am ) ) . V.toList
      compareAsc  f (_,a) (_,b) = comparing f a b
      compareDesc f (_,a) (_,b) = comparing f b a
  let sTitle :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sTitle am so aids = V.fromList ( fst <$> sortBy (comp so) (sortAsi am aids)) where
        comp o = case o of
                   Asc  -> compareAsc (fmap albumTitle)
                   Desc -> compareDesc (fmap albumTitle)
  let sArtist :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sArtist am so aids = V.fromList ( fst <$> sortBy (comp so) (sortAsi am aids)) where
        comp o = case o of
                   Asc  -> compareAsc (fmap albumArtist)
                   Desc -> compareDesc (fmap albumArtist)
  let sAdded :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sAdded am so aids = V.fromList ( fst <$> sortBy (comp so) (sortAsi am aids)) where
        comp o = case o of
                   Asc  -> compareDesc (fmap albumAdded)
                   Desc -> compareAsc (fmap albumAdded)
  let sfs :: Map Text (Map Int Album -> SortOrder -> Vector Int -> Vector Int) -- sort functions
      sfs = M.fromList [ ( "Default", sDef    )
                       , ( "Artist",  sArtist )
                       , ( "Title",   sTitle  )
                       , ( "Added",   sAdded  )
                       ]

      getSort' :: Map Int Album -> Text -> (SortOrder -> Vector Int -> Vector Int)
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
  let allLocs = updateLocations lm

  lnr <- newIORef listNames'
  lr <- newIORef lists'
  lo <- newIORef allLocs
  dr <- newIORef dc
  ar <- newIORef albums'
  sr <- newIORef "Default"
  so <- newIORef Asc
  return Env { discogsR = dr
             , albumsR = ar, listsR = lr, locsR = lo
             , listNamesR = lnr
             , sortNameR = sr
             , sortOrderR = so
             , sorts = sorts'
             , url = "/"
             , getList = getList'
             , getSort = getSort'
             }

