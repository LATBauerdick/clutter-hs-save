{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FromJSON ( Release (..)
                , readReleases
                , readDLists
                , readDFolders
                ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F

import Data.Aeson ( (.:), (.:?), (.!=), FromJSON(..), withObject, eitherDecode)
import qualified Data.Vector as V (Vector, fromList, toList )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe  )
import Data.Text (Text)
import Data.Either (fromRight)



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
      Right ds -> print $ drop (length ds-4) ds
    return $ fromRight [] d

readDLists :: Text -> IO ( V.Vector Int )
readDLists l = do
    let fn :: FilePath
        fn = "data/lists.json"
    d <- (eitherDecode <$> BL.readFile fn) :: IO (Either String [DLists])
    case d of
      Left err -> putStrLn err
      Right ds -> print ds
    let ds = V.fromList $ fromRight [] d
    F.for_ ds print

    -- let lists :: M.Map Text Int
    let lm :: M.Map Text Int
        lm = M.fromList [ (t,i) | DLists i t <- V.toList ds ]

    let lid = fromMaybe 540434 ( M.lookup l lm )
    let fn = "data/l" ++ show lid ++ ".json"
    readDAids fn

  -- vl <- readDLists "Listened"
  -- vp <- readDFolders "Piano"
  -- vf <- readDFolders "Pop"
  -- let lists :: M.Map Text ( Vector Int ) -- map of albumID lists
  --     lists = M.fromList [ ( "All",      va )
  --                        , ( "Discogs",  V.map albumID vda )
  --                        , ( "Tidal",    V.map albumID vta )
  --                        , ( "Listened", vl )
  --                        , ( "Piano",    vp )
  --                        , ( "Pop",      vf )
  --                        ]
readDFolders :: Text -> IO ( V.Vector Int )
readDFolders f = do
    let fn :: FilePath
        fn = "data/folders.json"
    d <- (eitherDecode <$> BL.readFile fn) :: IO (Either String [DLists])
    case d of
      Left err -> putStrLn err
      Right ds -> print ds
    let ds = V.fromList $ fromRight [] d
    F.for_ ds print

    -- let folders :: M.Map Text Int
    let fm :: M.Map Text Int
        fm = M.fromList [ (t,i) | DLists i t <- V.toList ds ]

    let fid = fromMaybe 1349997 ( M.lookup f fm )
    let fn = "data/f" ++ show fid ++ ".json"
    readDAids fn


newtype DAid = DAid { dlaid   :: Int } deriving (Show)
instance FromJSON DAid where
  parseJSON = withObject "daid" $ \ o -> do
    d_   <- o .: "id"
    return $ DAid d_

readDAids :: FilePath -> IO ( V.Vector Int )
readDAids fn = do
  d <- (eitherDecode <$> BL.readFile fn) :: IO (Either String [DAid])
  case d of
    Left err -> putStrLn err
    Right ds -> print ds
  let ds = V.fromList $ fromRight [] d
      aids  = dlaid <$> ds
  return aids
