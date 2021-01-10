{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Env
    ( Env (..)
    , testEnv
    , initEnv
    ) where
import Prelude hiding ( (++) )

-- import Control.Monad.IO.Class ( liftIO )
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Foldable as F
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
                , readDLists
                , readDFolders
                , readAlbums
                )



----------------------- Provider stuff

data Env
  = Env
  { albums      :: M.Map Int Album
  -- , currentList :: Vector Int
  , albumList   :: Vector Album
  -- , sort        :: Vector Int -> Vector Int
  , url         :: Text
  , lists       :: Vector Text
  }

-- sortAdded :: M.Map Int Album -> Vector Int -- reverse chronological
-- sortAdded am = V.fromList $ fst <$> sortBy ( \ (_,a) (_,b) -> comparing albumAdded b a ) ( M.toList am)
-- aidTitles :: (M.Map Int Album) -> Vector Int
-- aidTitles am = V.fromList $ fst <$> (sortBy ( \ (_,a) (_,b) -> comparing albumTitle a b ) $ M.toList am)
-- aidArtists :: (M.Map Int Album) -> Vector Int
-- aidArtists am = V.fromList $ fst <$> (sortBy ( \ (_,a) (_,b) -> comparing albumArtist a b ) $ M.toList am)

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

  let va = V.map albumID vaa
  let thisAlbumMap = M.fromList $ map (\ a -> (albumID a, a)) (V.toList vaa)


  vl <- readDLists "Listened"
  vp <- readDFolders "Piano"
  vf <- readDFolders "Pop"
  let lists :: M.Map Text ( Vector Int ) -- map of albumID lists
      lists = M.fromList [ ( "All",      va )
                         , ( "Discogs",  V.map albumID vda )
                         , ( "Tidal",    V.map albumID vta )
                         , ( "Listened", vl )
                         , ( "Piano",    vp )
                         , ( "Pop",      vf )
                         ]
      getList :: Text -> Vector Int
      getList t = fromMaybe V.empty (M.lookup t lists)

-- return the list of tuples (albumID, Maybe Album), unsorted
  let asi :: Vector Int -> [ ( Int, Maybe Album ) ]
      asi aids =  map ( \aid -> ( aid, M.lookup aid thisAlbumMap ) ) $ V.toList aids
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

  let thisList :: Vector Int
      thisList = getList "Listened"

  let thisSort :: Vector Int -> Vector Int
      thisSort = getSf "Title"

  let thisAlbumList :: Vector Album
      thisAlbumList = V.fromList $ mapMaybe ( `M.lookup` thisAlbumMap ) ( V.toList (thisSort thisList) )


  return $ Env { albums = thisAlbumMap, albumList = thisAlbumList, url = "http://localhost:8080/" }

