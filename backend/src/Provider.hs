{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Provider ( Album (..)
                , Tidal (..)
                , Discogs (..)
                , readDLists
                , readDFolders
                , readAlbums
                , atest
                ) where

import FromJSON ( Release (..)
                , readReleases
                , readDLists
                , readDFolders
                )

import Data.Maybe (fromMaybe)
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as BL
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ

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

    putStrLn $ "Total # Albums: " ++ show (length as)
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

    putStrLn $ "Total # Albums: " ++ show (length as)
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


catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show



cmdqq :: Text
cmdqq = [r|

  get raw JSON from discogs and pre-process

curl "https://api.discogs.com/users/LATB/collection/folders/0/releases?page=1&per_page=500" -H "Authorization: Discogs token=<token>" > data/draw1.json
curl "https://api.discogs.com/users/LATB/collection/folders/0/releases?page=2&per_page=500" -H "Authorization: Discogs token=<token>" > data/draw2.json
curl "https://api.discogs.com/users/LATB/collection/folders/0/releases?page=3&per_page=500" -H "Authorization: Discogs token=<token>" > data/draw3.json
cat data/draw*.json | jq -s '[ .[].releases[] | {id: .id, title: .basic_information.title, artists: [ .basic_information.artists[].name], released: .basic_information.year|tostring, added: .date_added, cover: .basic_information.cover_image, folder: .folder_id }]' > data/dall.json

 # get list of Folders and pre-process
curl "https://api.discogs.com/users/LATB/collection/folders" -H "Authorization: Discogs token=<token>"
cat data/folders-raw.json | jq -s '[ .[].folders[] | { id: .id, name: .name }]' > data/folders.json

# get aids for folder and pre-process
curl "https://api.discogs.com/users/LATB/collection/folders/1349997/releases?sort=added&sort_order=desc&page=1&per_page=500" -H "Authorization: Discogs token=<token>" > data/f1349997-raw.json
cat data/f1349997-raw.json | jq -s '[ .[].releases[] | { id: .id  }]' > data/f1349997.json

# get list of Lists and pre-process
curl "https://api.discogs.com/users/LATB/lists" -H "Authorization: Discogs token=<token>"  > data/lists-raw.json
cat data/lists-raw.json | jq -s '[ .[].lists[] | { id: .id, name: .name }]' > data/lists.json

# get aids from list and pre-process
curl "https://api.discogs.com/lists/540434" -H "Authorization: Discogs token=<token>"   > data/l540434-raw.json
cat data/l541650-raw.json | jq -s '[ .[].items[] | { id: .id  }]' > data/l541650.json

# get raw JSON from Tidal and pre-process

curl https://api.tidalhifi.com/v1/users/45589625/favorites/albums/\?sessionId\=<session-id>\&countryCode\=US\&limit\=2999 > data/traw.json
cat data/traw.json | jq -s '[ .[].items[] | { id: .item.id, title: .item.title, artists: [ .item.artists[].name ], released: .item.releaseDate, added: .created, cover: .item.cover, Folder: 999 } ]' > data/tall.json







# get release <id>
curl "https://api.discogs.com/releases/8807550" -H "Authorization: Discogs token=<token>"  | jq --color-output -r '.' | bat -n

# get all releases from my list <id> ("Listened")
curl "https://api.discogs.com/lists/540434" -H "Authorization: Discogs token=<token>"  | jq --color-output -r '.items[0]' | bat -n

# get all releases in folder Pop
curl "https://api.discogs.com/users/LATB/collection/folders/1349997/releases" -H "Authorization: Discogs token=<token>"  | jq --color-output -r '.releases[] | {id: .id, title: .basic_information.title, artist: .basic_information.artists[0].name, added: .date_added } | join ("\t")' | bat -n





|]


