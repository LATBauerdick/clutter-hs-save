{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}

module FromJSON ( Release (..)
                , readReleases
                , readLists
                , readFolders
                ) where
import Relude

import Data.Aeson ( (.:), (.:?), (.!=), FromJSON(..), withObject, eitherDecode)
import Data.Vector ( Vector )
import qualified Data.Vector as V (fromList, toList )
import qualified Data.Map as M
import Control.Exception (IOException)
import qualified Control.Exception as Exception

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
    d <- (eitherDecode <$> readFileLBS fn) :: IO (Either String [Release])
    case d of
      Left err -> putTextLn $ toText err
      Right ds -> pure () -- print $ drop (length ds-4) ds
    return $ fromRight [] d


readLists :: IO ( M.Map Text ( Int, Vector Int ) )
readLists = do

-- read the names and IDs of my Discogs lists or folders
    let readNameIds :: FilePath -> IO ( Vector DLists )
        readNameIds fn = do
          d <- (eitherDecode <$> readFileLBS fn) :: IO (Either String [DLists])
          case d of
            Left err -> putTextLn $ toText err
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
          let fn = "data/l" <> show i <> ".json"
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
          d <- (eitherDecode <$> readFileLBS fn) :: IO (Either String [DLists])
          case d of
            Left err -> putTextLn $ toText err
            Right _ -> pure () -- print ds
          let ds = V.fromList $ fromRight [] d
          putTextLn "---------------readFolders------------"
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
  d <- (eitherDecode <$> readFileLBS fn) :: IO (Either String [DAid])
  case d of
    Left err -> putTextLn $ toText err
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

