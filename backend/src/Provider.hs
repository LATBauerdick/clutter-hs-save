{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Provider ( Album (..)
                , Tidal (..)
                , FT.TidalInfo (..)
                , Discogs (..)
                , FD.DiscogsInfo (..)
                , readListAids
                , readAlbums
                , readLists
                , readFolders
                , readFolderAids
                , refreshLists
                , atest
                ) where
import Relude

import FromJSON ( Release (..) )
import qualified FromJSON as FJ ( readReleases
                                , readLists
                                , readFolders
                                )
import qualified FromTidal as FT ( readTidalReleases, TidalInfo (..) )
import qualified FromDiscogs as FD ( readDiscogsReleases
                                   , readDiscogsLists
                                   , readListAids
                                   , readDiscogsFolders
                                   , refreshLists
                                   , DiscogsInfo (..)
                                   )
import Data.Maybe (fromMaybe)
-- import Data.Text.Encoding ( decodeUtf8 )
import qualified Text.Show
import qualified Data.Map.Strict as M
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T


data Album
  = Album
  { albumID       :: Int
  , albumTitle    :: Text
  , albumArtist   :: Text
  , albumReleased :: Text
  , albumCover    :: Text
  , albumAdded    :: Text
  , albumFolder   :: Int
  , albumURL      :: Album -> Text
  }
instance Eq Album where
  (==) a b = albumID a == albumID b
instance Show Album where
  show a = "Album {albumID = " ++ show (albumID a) ++ ", albumTitle =" ++ show (albumTitle a) ++ "}"

ppp :: Tidal
ppp = Tidal $ FT.TidalFile "xxx"
atest :: [ Album ]
atest  = [ Album 161314 "Mezzanine" "Massive Attack" "2001" "161314.jpg" "2018-01-01T18:01:42-08:00" 1349997 (const "https://www.tidal.com/album/161314")
         , Album 5253301 "Beethoven - Symphonie Nr. 3 »Eroica« & 4" "Herbert von Karajan" "1992" "5253301.jpg" "2017-09-17T20:57:52-07:00" 1351871  (const "https://www.tidal.com/album/5253301")
         ]

class Provider p where
  readAlbums :: p -> IO (Vector Album)
  readLists :: p ->  IO ( Map Text (Int, Vector Int ) )

newtype Tidal = Tidal FT.TidalInfo
getTidal :: Tidal -> FT.TidalInfo
getTidal (Tidal ti) = ti

newtype Discogs = Discogs FD.DiscogsInfo deriving Show
getDiscogs :: Discogs -> FD.DiscogsInfo
getDiscogs (Discogs di) = di

instance Provider Tidal where
  readLists _ = undefined
  readAlbums p = do
    let
        toCoverURL r = T.concat [
              T.pack "https://resources.tidal.com/images/"
            , T.intercalate "/" $ T.splitOn "-" (dcover r)
            , T.pack "/320x320.jpg" ]
        toFolder = 2
        getAlbumURL :: Album -> Text
        getAlbumURL a = T.pack $
            "https://www.tidal.com/album/" ++ show ( albumID a )
        toAlbum r = Album (daid r)
                          (dtitle r)
                          (T.intercalate ", " $ dartists r)
                          (dreleased r)
                          (toCoverURL r)
                          (dadded r)
                          toFolder
                          getAlbumURL
    ds <- case getTidal p of
          FT.TidalFile fn -> FJ.readReleases fn
          _ -> FT.readTidalReleases (getTidal p)
    let as  = toAlbum <$> ds

    putTextLn $ "Total # Tidal Albums: " <> show (length as)
    -- print $ drop (length as - 4) as

    return $ V.fromList as


instance Provider Discogs where
  readLists p = case getDiscogs p of
                  FD.DiscogsFile fn -> FJ.readLists
                  _ -> FD.readDiscogsLists (getDiscogs p)
  readAlbums p = do
    let
        toCoverURL r = dcover r
        toFolder r = dfolder r -- fromMaybe "Nothing" $ M.lookup (dfolder r) fm
          where fm :: Map Int Text
                fm = M.fromList [ ( 1349997, "Pop" )
                                , ( 1351871, "Symphonic" )
                                , ( 1351873, "Concertos" )
                                , ( 1351869, "Piano" )
                                , ( 1350005, "Opera&Vocal" )
                                , ( 1351883, "Chamber" )
                                , ( 1254070, "Basement" )
                                , ( 2,       "Tidal" )
                                ]
        getAlbumURL a = T.pack $
            "https://www.discogs.com/release/" ++ show ( albumID a )
        toAlbum r = Album (daid r)
                          (dtitle r)
                          (T.intercalate ", " $ dartists r)
                          (dreleased r)
                          (toCoverURL r)
                          (dadded r)
                          (toFolder r)
                          getAlbumURL
        fn = "data/dall.json"

    ds <- case getDiscogs p of
          FD.DiscogsFile fn -> FJ.readReleases fn
          _ -> FD.readDiscogsReleases (getDiscogs p)

    let as  = toAlbum <$> ds

    putTextLn $ "Total # Discogs Albums: " <> show (length as)
    -- print $ drop ( length as - 4 ) as

    return $ V.fromList as


readListAids :: Discogs -> Int -> IO ( Vector Int )
readListAids p i = case getDiscogs p of
                     FD.DiscogsFile _ -> pure V.empty -- maybe not ok
                     _ -> FD.readListAids (getDiscogs p) i

readFolders :: Discogs -> IO ( Map Text Int )
readFolders p = case getDiscogs p of
                  FD.DiscogsFile fn -> FJ.readFolders
                  _ -> FD.readDiscogsFolders (getDiscogs p)

-- populate the aids for folders from the folder+id in each Album
readFolderAids :: Map Text Int -> Map Int Album -> Map Text ( Int, Vector Int )
readFolderAids fm am = fam where
  -- 0: all discogs
  -- 1: uncategorized
  fam'  = M.mapWithKey (getFolder am) fm
  fam = M.insert "Tidal" (2, allTidal am)
      $ M.insert "Discogs" (0, allDiscogs am)
      $ M.insert "All" (0, all am)
        fam'
  all am = sAdded am
         $ V.map albumID
         $ V.fromList $ M.elems am
  allDiscogs am = sAdded am
                 $ V.map fst
                 $ V.filter (\ (_,f) -> f/=2)
                 $ V.map (\a -> (albumID a, albumFolder a))
                 $ V.fromList $ M.elems am
  allTidal am = sAdded am
              $ V.map fst
              $ V.filter (\ (_,f) -> f==2)
              $ V.map (\a -> (albumID a, albumFolder a))
              $ V.fromList $ M.elems am

  sAdded :: Map Int Album -> Vector Int -> Vector Int
  sAdded am aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumAdded) b a ) ( asi am aids )) where
    asi :: Map Int Album -> Vector Int -> [ ( Int, Maybe Album ) ]
    asi am aids =  map ( \aid -> ( aid, M.lookup aid am ) ) $ V.toList aids
  getFolder :: Map Int Album -> Text -> Int -> (Int, Vector Int)
  getFolder am n i = (i, filtFolder i)
  filtFolder :: Int -> Vector Int
  filtFolder fid = sAdded am
                 $ V.map fst
                 $ V.filter (\ (_,f) -> f==fid)
                 $ V.map (\a -> (albumID a, albumFolder a))
                 $ V.fromList $ M.elems am

refreshLists :: Discogs -> IO ( Map Text (Int, Vector Int) )
refreshLists p = case getDiscogs p of
                  FD.DiscogsFile fn -> undefined
                  _ -> FD.refreshLists (getDiscogs p)
-- items[].item.type
-- "SINGLE"
-- "ALBUM"
-- "EP"

-- link : http://www.tidal.com/album/aid
--             https://listen.tidal.com/album/
--             https://www.discogs.com/release/

-- items[].item.audioQuality
-- LOSSLESS
-- HI_RES
-- HIGH

