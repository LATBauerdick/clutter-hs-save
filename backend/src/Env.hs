{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Env
    ( Env (..)
    , testEnv
    , initEnv
    ) where


-- import Control.Monad.IO.Class ( liftIO )
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Foldable as F
-- import Data.Maybe ( mapMaybe )
import qualified Data.Vector as V ( Vector (..), fromList, singleton )
import Data.List (sortBy)
import Data.Ord (comparing)

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
-- import qualified Lucid as L
-- import Network.HTTP.Media ((//), (/:))
import FromJSON ( Tidal (..)
                , Discogs (..)
                , Album (..)
                , readDLists
                , readDFolders
                , albumMap
                )



----------------------- Provider stuff

data Env
  = Env
  { albums :: M.Map Int Album
  , aidAdded :: V.Vector Int
  }

sortAdded :: M.Map Int Album -> V.Vector Int -- reverse chronological
sortAdded am = V.fromList $ fst <$> sortBy ( \ (_,a) (_,b) -> comparing albumAdded b a ) ( M.toList am)
-- aidTitles :: (M.Map Int Album) -> V.Vector Int
-- aidTitles am = V.fromList $ fst <$> (sortBy ( \ (_,a) (_,b) -> comparing albumTitle a b ) $ M.toList am)
-- aidArtists :: (M.Map Int Album) -> V.Vector Int
-- aidArtists am = V.fromList $ fst <$> (sortBy ( \ (_,a) (_,b) -> comparing albumArtist a b ) $ M.toList am)

testEnv :: Env
testEnv = Env { albums = M.singleton 1 testAlbum, aidAdded = V.singleton ( albumID testAlbum ) }

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
  am1 <- albumMap $ Discogs "data/dall.json"
  am2 <- albumMap $ Tidal "data/tall.json"
  let am = M.unions [ am0, am1, am2 ]

  vl <- readDLists
  vf <- readDFolders

  return $ Env { albums = am, aidAdded = snd vf } -- sortAdded am }


