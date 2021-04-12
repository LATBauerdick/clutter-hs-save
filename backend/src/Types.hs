{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Types ( TagFolder (..)
             , TidalInfo (..)
             , Release (..)
             , Album (..)
             ) where

import Relude
import qualified Text.Show

data TidalInfo = TidalFile FilePath | TidalSession Int Text Text

class ATags f where toInt :: f -> Int
data TagFolder = TDiscogs | TNotUsed | TTidal | TAll
    deriving (Enum, Read, Show, Eq, Ord)
instance ATags TagFolder where
    toInt = fromEnum

data Release
  = Release
  { daid      :: Int
  , dtitle    :: !Text
  , dartists  :: [Text]
  , dreleased :: !Text
  , dadded    :: !Text
  , dcover    :: !Text
  , dfolder   :: Int
  , dformat   :: [Text]
  , dtidalurl :: Maybe Text
  } deriving (Show)

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
  , albumFormat   :: Text
  , albumTidal    :: Maybe Text
  }
instance Eq Album where
  (==) a b = albumID a == albumID b
instance Show Album where
  show a = "Album {albumID = " ++ show (albumID a) ++ ", albumTitle =" ++ show (albumTitle a) ++ "}"

