{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Types ( Tidal (..), getTidal
             , Discogs (..), getDiscogs
             , DiscogsInfo (..)
             , TagFolder (..)
             , TidalInfo (..)
             , Release (..)
             , Album (..)
             , SortOrder (..)
             , Env (..)
             ) where

import Relude
import qualified Text.Show
import Data.Vector ( Vector )

data TidalInfo = TidalFile FilePath | TidalSession Int Text Text
data DiscogsInfo = DiscogsFile FilePath | DiscogsSession Text Text
  deriving Show

class ATags f where toInt :: f -> Int
data TagFolder = TDiscogs | TNotUsed | TTidal | TAll
    deriving (Enum, Read, Show, Eq, Ord)
instance ATags TagFolder where
    toInt = fromEnum

data SortOrder = Asc | Desc
  deriving (Enum, Read, Show, Eq, Ord)

newtype Tidal = Tidal TidalInfo
getTidal :: Tidal -> TidalInfo
getTidal (Tidal ti) = ti

newtype Discogs = Discogs DiscogsInfo deriving Show
getDiscogs :: Discogs -> DiscogsInfo
getDiscogs (Discogs di) = di

data Env
  = Env
  { albums      :: IORef ( Map Int Album )
  , listNames   :: IORef ( Vector Text )
  , lists       :: IORef ( Map Text (Int, Vector Int) )
  , sortName    :: IORef Text
  , sortOrder   :: IORef SortOrder
  , sorts       :: Vector Text
  , url         :: Text
  , discogs     :: IORef Discogs
  , getList     :: Env -> Text -> IO ( Vector Int )
  , getSort     :: Map Int Album -> Text -> (SortOrder -> Vector Int -> Vector Int )
  }

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

