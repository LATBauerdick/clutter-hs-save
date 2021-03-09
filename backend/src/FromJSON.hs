{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module FromJSON ( Release (..)
                , readReleases
                , readLists
                , readFolders
                ) where
import Relude

import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F

import Data.Aeson ( (.:), (.:?), (.!=), FromJSON(..), withObject, eitherDecode)
import Data.Vector ( Vector )
import qualified Data.Vector as V (fromList, toList )
import Data.Traversable ( traverse )
import qualified Data.Map as M
import Data.Text (Text)
import Data.Either (fromRight)

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Text.RawString.QQ


data Release
  = Release
  { daid      :: Int
  , dtitle    :: !Text
  , dartists  :: [ Text ]
  , dreleased :: !Text
  , dadded    :: !Text
  , dcover    :: !Text
  , dfolder   :: Int
  -- , dnotes   :: [ DNote ]
  } deriving (Show)
data DNote
  = DNote
  { dfid  :: Int
  , dval  :: !Text
  } deriving (Show)


instance FromJSON DNote where
  parseJSON = withObject "notes" $ \ o -> do
    dfid_ <- o .: "field_id"
    dval_ <- o .: "value"
    return $ DNote dfid_ dval_

instance FromJSON Release where
  parseJSON = withObject "release" $ \ o -> do
    daid_      <- o .: "id"
    dtitle_    <- o .: "title"
    dartists_  <- o .: "artists"
    dreleased_ <- o .: "released"
    dadded_    <- o .: "added"
    dcover_    <- o .:? "cover" .!= ""
    dfolder_   <- o .: "folder"
    -- dnotes_    <- o .: "notes"
    return $ Release daid_ dtitle_ dartists_ dreleased_ dadded_ dcover_ dfolder_ -- dnotes_

data DLists
  = DLists
  { dlid   :: Int
  , dlname :: !Text
  } deriving (Show)
instance FromJSON DLists where
  parseJSON = withObject "lists" $ \ o -> do
    dlid_   <- o .: "id"
    dlname_ <- o .: "name"
    return $ DLists dlid_ dlname_

readReleases :: FilePath -> IO [ Release ]
readReleases fn =do
    d <- (eitherDecode <$> BL.readFile fn) :: IO (Either String [Release])
    case d of
      Left err -> putStrLn err
      Right ds -> pure () -- print $ drop (length ds-4) ds
    return $ fromRight [] d


readLists :: IO ( M.Map Text ( Int, Vector Int ) )
readLists = do

-- read the names and IDs of my Discogs lists or folders
    let readNameIds :: FilePath -> IO ( Vector DLists )
        readNameIds fn = do
          d <- (eitherDecode <$> BL.readFile fn) :: IO (Either String [DLists])
          case d of
            Left err -> putStrLn err
            Right _ -> pure () -- print ds
          let ds = V.fromList $ fromRight [] d
          -- F.for_ ds print
          return ds

-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs arf unique
-- NB: the JSON required to extract album id info ir different between them
    let readListAids :: DLists -> IO ( Text, (Int, Vector Int ))
        readListAids ( DLists i t ) = do
          let fn = "data/l" ++ show i ++ ".json"
          aids <- readDAids fn
          return ( t, (i, aids) )

    ls <- readNameIds "data/lists.json"
    fs <- readNameIds "data/folders.json"
    lm <- traverse readListAids ( ls <> fs )

    return $ M.fromList ( V.toList lm )

-- read the names and IDs of my Discogs lists or folders
readFolders :: IO ( M.Map Text Int )
readFolders = do
    let readNameIds :: FilePath -> IO ( Vector DLists )
        readNameIds fn = do
          d <- (eitherDecode <$> BL.readFile fn) :: IO (Either String [DLists])
          case d of
            Left err -> putStrLn err
            Right _ -> pure () -- print ds
          let ds = V.fromList $ fromRight [] d
          putStrLn "---------------readFolders------------"
          -- F.for_ ds print
          return ds

    -- ls <- readNameIds "data/lists.json"
    fs <- readNameIds "data/folders.json"
    let fns = (\ (DLists i n) -> (n, i)) <$> fs
    return $ M.fromList ( V.toList fns )

newtype DAid = DAid { dlaid   :: Int } deriving (Show)
instance FromJSON DAid where
  parseJSON = withObject "daid" $ \ o -> do
    d_   <- o .: "id"
    return $ DAid d_

readDAids :: FilePath -> IO ( Vector Int )
readDAids fn = do
  d <- (eitherDecode <$> BL.readFile fn) :: IO (Either String [DAid])
  case d of
    Left err -> putStrLn err
    Right _ -> pure () -- print ds
  let ds = V.fromList $ fromRight [] d
      aids  = dlaid <$> ds
  return aids



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
cat data/traw.json | jq -s '[ .[].items[] | { id: .item.id, title: .item.title, artists: [ .item.artists[].name ], released: .item.releaseDate, added: .created, cover: .item.cover, Folder: 2 } ]' > data/tall.json







# get release <id>
curl "https://api.discogs.com/releases/8807550" -H "Authorization: Discogs token=<token>"  | jq --color-output -r '.' | bat -n

# get all releases from my list <id> ("Listened")
curl "https://api.discogs.com/lists/540434" -H "Authorization: Discogs token=<token>"  | jq --color-output -r '.items[0]' | bat -n

# get all releases in folder Pop
curl "https://api.discogs.com/users/LATB/collection/folders/1349997/releases" -H "Authorization: Discogs token=<token>"  | jq --color-output -r '.releases[] | {id: .id, title: .basic_information.title, artist: .basic_information.artists[0].name, added: .date_added } | join ("\t")' | bat -n





|]

