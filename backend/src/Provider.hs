{-# LANGUAGE OverloadedStrings #-}


module Provider ( Album (..)
                , Tidal (..)
                , Discogs (..)
                -- , readDLists
                -- , readDFolders
                , readAlbums
                , readLists
                , atest
                ) where

import FromJSON ( Release (..)
                , readReleases
                , readLists
                -- , readDLists
                -- , readDFolders
                )

import Data.Maybe (fromMaybe)
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Map.Strict as M
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
  , albumFolder   :: Text
, albumURL      :: Album -> Text
  }
instance Eq Album where
  (==) a b = albumID a == albumID b
instance Show Album where
  show a = "Album {albumID = " ++ show (albumID a) ++ ", albumTitle =" ++ show (albumTitle a) ++ "}"

ppp :: Tidal
ppp = Tidal "xxx"
atest :: [ Album ]
atest  = [ Album 161314 "Mezzanine" "Massive Attack" "2001" "161314.jpg" "2018-01-01T18:01:42-08:00" "Pop" ( getAlbumURL ppp )
         , Album 5253301 (decodeUtf8 "Beethoven - Symphonien Nr. 3 »Eroica« & 4") "Herbert von Karajan" "1992" "5253301.jpg" "2017-09-17T20:57:52-07:00" "Symphonic" ( getAlbumURL ppp )
         ]

class Provider p where
  toCoverURL :: p -> Release -> Text
  toAlbum :: p -> Release -> Album
  toFolder :: p -> Release -> Text
  getAlbumURL :: p -> Album -> Text
  readAlbums :: p -> IO (V.Vector Album)

newtype Tidal
  = Tidal FilePath
newtype Discogs
  = Discogs FilePath

instance Provider Tidal where

  toCoverURL _ r = T.concat [ T.pack "https://resources.tidal.com/images/", T.intercalate "/" $ T.splitOn "-" (dcover r), T.pack "/320x320.jpg" ]

  toFolder _ _ = "Tidal"

  toAlbum p r = Album (daid r) (dtitle r) (T.intercalate ", " $ dartists r) (dreleased r) (toCoverURL p r) (dadded r) (toFolder p r) (getAlbumURL p)

  getAlbumURL _ a = T.pack $ "https://www.tidal.com/album/" ++ show ( albumID a )

  readAlbums p = do
    let fn :: FilePath
        fn = "data/tall.json"
    ds <- readReleases fn
    let as  = toAlbum p <$> ds

    putStrLn $ "Total # Tidal Albums: " ++ show (length as)
    print $ drop (length as - 4) as
    return $ V.fromList as
    -- return $ M.fromList $ map (\ a -> (albumID a, a)) as

instance Provider Discogs where

  toCoverURL _ r = dcover r

  toFolder _ r = fromMaybe "Nothing" $ M.lookup (dfolder r) fm
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
  toAlbum p r = Album (daid r) (dtitle r) (T.intercalate ", " $ dartists r) (dreleased r) (toCoverURL p r) (dadded r) (toFolder p r) (getAlbumURL p)

  getAlbumURL _ a = T.pack $ "https://www.discogs.com/release/" ++ show ( albumID a )

  readAlbums p = do
    let fn :: FilePath
        fn = "data/dall.json"
    ds <- readReleases fn
    let as  = toAlbum p <$> ds

    putStrLn $ "Total # Discogs Albums: " ++ show (length as)
    print $ drop ( length as - 4 ) as
    return $ V.fromList as
    -- return $ M.fromList $ map (\ a -> (albumID a, a)) as

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

