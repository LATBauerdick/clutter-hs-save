{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Env
    ( Env (..)
    , testEnv
    , initEnv
    ) where
import Prelude hiding ( (++) )

import Data.Maybe ( mapMaybe, fromMaybe )
import Data.Vector ( Vector (..), (++) )
import qualified Data.Vector as V ( fromList, toList, empty, singleton, map)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
-- import qualified Lucid as L
-- import Network.HTTP.Media ((//), (/:))
import Provider ( Tidal (..)
                , Discogs (..)
                , Album (..)
                , readAlbums
                , readLists
                )

data Env
  = Env
  { albums      :: M.Map Int Album
  , lists       :: Vector Text
  , getList     :: Text -> Vector Int
  , sorts       :: Vector Text
  , getSort     :: Text -> ( Vector Int -> Vector Int )
  , url         :: Text
  }

testEnv :: Env
testEnv = Env { albums = M.singleton 1 testAlbum, url = "http://localhost:8080/" }

testAlbum :: Album
testAlbum = Album 123123
                  "Test Title"
                  "Test Artists"
                  "2021"
                  "https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg"
                  "2021-01-01T01:23:01-07:00" "Pop" ( const "xxx" )

initEnv :: IO Env
initEnv = do
  let am0 = M.singleton 1 testAlbum
  vda <- readAlbums $ Discogs "data/dall.json"
  vta <- readAlbums $ Tidal "data/tall.json"
  let vaa :: Vector Album
      vaa = vda ++ vta

  let myAlbumMap = M.fromList $ map (\ a -> (albumID a, a)) (V.toList vaa)

-- define sort functions
  let asi :: Vector Int -> [ ( Int, Maybe Album ) ]
      asi aids =  map ( \aid -> ( aid, M.lookup aid myAlbumMap ) ) $ V.toList aids
  let sDef :: Vector Int -> Vector Int
      sDef l = l
  let sAdded :: Vector Int -> Vector Int
      sAdded env = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumAdded) b a ) ( asi env ))
  let sArtist :: Vector Int -> Vector Int
      sArtist env = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumArtist) a b ) ( asi env ))
  let sTitle :: Vector Int -> Vector Int
      sTitle env = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumTitle) a b ) ( asi env ))
  let sfs :: M.Map Text (Vector Int -> Vector Int) -- sort functions
      sfs = M.fromList [ ( "Default", sDef    )
                       , ( "Artist",  sArtist )
                       , ( "Title",   sTitle  )
                       , ( "Added",   sAdded  )
                       ]
      getSf :: Text -> (Vector Int -> Vector Int)
      getSf t = fromMaybe sDef (M.lookup t sfs)
      sns = V.fromList $ M.keys sfs


-- read the map of Discogs lists and folders
  lm <- readLists
-- add the Tidal, Discogs, and the All lists "by hand"
-- and sort them ("Default") for Date Added
  let ds = sAdded $ albumID <$> vda -- getList "All"
      ts = sAdded $ albumID <$> vta
      as = sAdded $ albumID <$> vaa

  let myLists = M.union ( M.fromList  [ ( "Discogs", ds ), ( "Tidal", ts ), ("All", as ) ] ) lm
      getList :: Text -> Vector Int
      getList ln = fromMaybe V.empty (M.lookup ln myLists )
      lists :: Vector Text
      lists = V.fromList $ M.keys myLists



  return $ Env { lists = lists, getList = getList, albums = myAlbumMap, url = "http://localhost:8080/", sorts = sns, getSort = getSf }

