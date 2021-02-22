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
import qualified Data.Vector as V ( fromList, toList, empty )
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T


-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Provider ( Tidal (..)
                , TidalInfo (..)
                , Discogs (..)
                , Album (..)
                , DToken (..)
                , readAlbums
                , readLists
                , refreshLists
                )

data Env
  = Env
  { albums      :: M.Map Int Album
  , listNames   :: Vector Text
  , lists       :: M.Map Text (Vector Int)
  , getList     :: Text -> Vector Int
  , sorts       :: Vector Text
  , getSort     :: Text -> ( Vector Int -> Vector Int )
  , url         :: Text
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
                  "2021-01-01T01:23:01-07:00" "Pop" ( const "xxx" )

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

-- return sorted list of albumIDs
  let sls :: Env -> Vector Album -> Text -> ( Text, Vector Int )
      sls env as n = ( n,  sAdded env ( albumID <$> as ) )

  let getList :: Env -> Text -> Vector Int
      getList env ln = fromMaybe V.empty (M.lookup ln (lists env))

      listNames :: Env -> Vector Text
      listNames env = V.fromList $ M.keys (lists env)

-- get Map ow all albums from Providers
  vda <- readAlbums $ Discogs "data/dall.json"
  -- vta <- readAlbums $ Tidal $ TidalFile "data/tall.json"
  t <- readFile "data/tok.dat"
  let userId = read ( words t !! 2 ) :: Int
      sessionId = T.pack $ words t !! 3
      countryCode = T.pack $ words t !! 4
  vta <- readAlbums $ Tidal $ TidalSession userId sessionId countryCode
-- read the map of Discogs lists and folders
  lm <- readLists $ Discogs "data/"

  let albums :: M.Map Int Album
      albums = M.fromList $
        (\ a -> (albumID a, a)) <$> V.toList ( vda <> vta )

-- add the Tidal, Discogs, and the All lists "by hand"
-- and sort them ("Default") for Date Added

  let lists :: M.Map Text ( Vector Int )
      lists = M.union ( M.fromList
                            [ sls env vda "Discogs"
                            , sls env vta "Tidal"
                            , sls env (vda <> vta) "All"
                            ] ) lm
                              where env = Env { albums = albums }

  let this = Env { albums = albums, lists = lists }
  return this { listNames = listNames this
              , getList = getList this
              , getSort = getSort this
              , sorts = sorts
              , url = "/"
              , token = undefined
              }

