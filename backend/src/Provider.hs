{-# LANGUAGE OverloadedStrings #-}


module Provider ( Album (..)
                , Tidal (..)
                , FT.TidalInfo (..)
                , Discogs (..)
                , FD.DiscogsInfo (..)
                , readListAids
                , DToken (..)
                , readAlbums
                , readLists
                , readFolders
                , refreshLists
                , atest
                ) where

import FromJSON ( Release (..) )
import qualified FromJSON as FJ ( readReleases, readLists )
import qualified FromTidal as FT ( readTidalReleases, TidalInfo (..) )
import qualified FromDiscogs as FD ( readDiscogsReleases
                                   , readDiscogsLists
                                   , readListAids
                                   , readDiscogsFolders
                                   , DiscogsInfo (..)
                                   )
import FromDiscogs ( DToken (..)
                   , refreshLists
                   )
import Data.Maybe (fromMaybe)
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Map.Strict as M
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Data.Text (Text)
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
         , Album 5253301 (decodeUtf8 "Beethoven - Symphonien Nr. 3 »Eroica« & 4") "Herbert von Karajan" "1992" "5253301.jpg" "2017-09-17T20:57:52-07:00" 1351871  (const "https://www.tidal.com/album/5253301")
         ]

class Provider p where
  readAlbums :: p -> IO (Vector Album)
  readLists :: p ->  IO ( M.Map Text (Int, Vector Int ) )

newtype Tidal = Tidal FT.TidalInfo
getTidal :: Tidal -> FT.TidalInfo
getTidal (Tidal ti) = ti

newtype Discogs = Discogs FD.DiscogsInfo
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
        toFolder = 999999
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

    putStrLn $ "Total # Tidal Albums: " ++ show (length as)
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
          where fm :: M.Map Int Text
                fm = M.fromList [ ( 1349997, "Pop" )
                                , ( 1351871, "Symphonic" )
                                , ( 1351873, "Concertos" )
                                , ( 1351869, "Piano" )
                                , ( 1350005, "Opera&Vocal" )
                                , ( 1351883, "Chamber" )
                                , ( 1254070, "Basement" )
                                , ( 999999,  "Tidal" )
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

    putStrLn $ "Total # Discogs Albums: " ++ show (length as)
    -- print $ drop ( length as - 4 ) as

    return $ V.fromList as


readListAids :: Discogs -> Int -> IO ( Vector Int )
readListAids p i = case getDiscogs p of
                     FD.DiscogsFile _ -> undefined
                     _ -> FD.readListAids (getDiscogs p) i

readFolders :: Discogs -> IO ( M.Map Text Int )
readFolders p = case getDiscogs p of
                  FD.DiscogsFile fn -> undefined
                  _ -> FD.readDiscogsFolders (getDiscogs p)
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

