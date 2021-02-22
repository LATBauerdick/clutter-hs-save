{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE TypeApplications #-}

module FromWeb  ( refreshLists
                    , readTidalReleases
                    , TidalInfo (..)
                    , DToken (..)
                    ) where

import qualified FromJSON as FJ ( Release (..) )

-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as S8
-- import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.ByteString.Lazy as BL ( readFile, putStrLn )
import qualified Data.Map as M
import Data.Vector ( Vector )
import qualified Data.Vector as V (fromList, toList, take )
import qualified Data.Foldable as F ( for_, traverse_  )
import Data.Either (fromRight)

-- import System.Process ( callCommand )

import Network.HTTP.Client ( Manager, defaultManagerSettings, newManager, httpLbs )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
-- import Network.HTTP.Types.Status (statusCode)


-- import Web.Authenticate.OAuth
import Data.Aeson ( (.:), (.:?), (.!=), FromJSON (..), withObject, eitherDecode, Value )
-- import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.Text as T ( unpack )
import GHC.Generics
-- import qualified Data.Yaml as Yaml

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.Proxy
import Servant
import Servant.API
import Servant.Client


data WTest = WTest { uri :: !Text
                     } deriving (Show, Generic)

data WLists = WLists { pagination :: WPagination
                     , lists :: [WList]
                     } deriving (Show, Generic)

data WPagination = WPagination { pages :: Int
                               , items :: Int
                               } deriving (Show, Generic)
data WList = WList  { id :: Int
                    , name :: !Text
                    } deriving (Show, Generic)

data WFolders = WFolders { folders :: [WList]
                         } deriving (Show, Generic)

newtype WLItems = WLItems { wlitems :: [WAid] } deriving (Show, Generic)
instance FromJSON WLItems where
  parseJSON = withObject "wlitems" $ \ o -> do
    d_ <- o .: "items"
    return $ WLItems d_

newtype WAid = WAid { wlaid :: Int } deriving (Show)
instance FromJSON WAid where
  parseJSON = withObject "waid" $ \ o -> do
    d_   <- o .: "id"
    return $ WAid d_

instance FromJSON WTest
instance FromJSON WLists
instance FromJSON WFolders
instance FromJSON WList
instance FromJSON WPagination

type Token = Text
type UserName = Text
type UserAgent = Text

type DiscogsAPI =
-- GET release
       "releases/249504"
       :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WTest
-- GET folders
  :<|> "users"
       :> Capture "name" UserName
       :> "collection/folders"
       :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WFolders
-- GET lists
  :<|> "users"
       :> Capture "name" UserName
       :> "lists"
       :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WLists
-- Get list items
  :<|> "lists"
       :> Capture "listid" Int
       :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WLItems

getTest ::  Maybe Token
           -> Maybe UserAgent
           -> ClientM WTest
getFolders :: UserName
           -> Maybe Token
           -> Maybe UserAgent
           -> ClientM WFolders
getLists :: UserName
         -> Maybe Token
         -> Maybe UserAgent
         -> ClientM WLists
getList :: Int
         -> Maybe Token
         -> Maybe UserAgent
         -> ClientM WLItems

discogsAPI :: Proxy DiscogsAPI
discogsAPI = Proxy

getTest :<|> getFolders :<|> getLists :<|> getList = client discogsAPI

data WTidal = WTidal
                { limit :: Int
                , totalNumberOfItems :: Int
                , items :: [WTItem]
                } deriving (Show, Generic)
data WTItem = WTItem
                { created :: !Text
                , item :: WTIItem
                } deriving (Show, Generic)
data WTIItem = WTIItem
                { id :: Int
                , title :: !Text
                , releaseDate :: !Text
                , url :: !Text
                , cover :: !Text
                , artists :: [WTArtist]
                } deriving (Show, Generic)
data WTArtist = WTArtist
                { id :: Int
                , name :: !Text
                } deriving (Show, Generic)
instance FromJSON WTidal
instance FromJSON WTItem
instance FromJSON WTIItem
instance FromJSON WTArtist


type TidalUserId = Int
type TidalSessionId = Text
type TidalCountryCode = Text
type TidalLimit = Int
type TidalAPI =
-- GET Tidal favorites
-- https://api.tidalhifi.com/v1/users/<userId>/favorites/albums?sessionId=<session-id>&countryCode=US&limit=2999
       "users"
       :> Capture "uid" TidalUserId
       :> "favorites" :> "albums"
       :> QueryParam "sessionId" TidalSessionId
       :> QueryParam "countryCode" TidalCountryCode
       :> QueryParam "limit" TidalLimit
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WTidal
tidalAPI :: Proxy TidalAPI
tidalAPI = Proxy
getTidal :: TidalUserId
         -> Maybe TidalSessionId
         -> Maybe TidalCountryCode
         -> Maybe TidalLimit
         -> Maybe UserAgent
         -> ClientM WTidal
getTidal = client tidalAPI

data TEnv = TEnv { userId :: TidalUserId
                 , sessionId :: TidalSessionId
                 , countryCode :: TidalCountryCode
                 , tlimit :: TidalLimit
                 , tclient :: ClientEnv
                 }
data TidalInfo = TidalFile FilePath | TidalSession Int Text Text
readTidalReleases :: TidalInfo -> IO [FJ.Release]
readTidalReleases ti = do
  manager <- newManager tlsManagerSettings  -- defaultManagerSettings
  let TidalSession uid sid cc = ti
      tenv :: TEnv
      tenv = TEnv { userId = uid
                  , sessionId = sid
                  , countryCode = cc
                  , tlimit = 9
                  , tclient = mkClientEnv manager ( BaseUrl Https "api.tidalhifi.com" 443 "v1" )
                  }
      tquery :: ClientM WTidal
      tquery  = getTidal (userId tenv)
                           ( Just (sessionId tenv) )
                           ( Just (countryCode tenv) )
                           ( Just (tlimit tenv) )
                           ( Just "ClutterApp/0.1" )
  res <- runClientM tquery ( tclient tenv )
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> pure ()
  let getReleases :: WTidal -> [FJ.Release]
      getReleases t = rs where
        WTidal {items=tis} = t
        getRelease :: WTItem -> FJ.Release
        getRelease ti = r where
          WTItem { created=tcreated, item=tii } = ti
          WTIItem { id = tid
                  , title = ttitle
                  , releaseDate = treleased
                  , url  = turl
                  , cover = tcover
                  , artists = tartists
                  } = tii
          as = (\ WTArtist { name=n } -> n ) <$> tartists
          r = FJ.Release  { daid      = tid
                          , dtitle    = ttitle
                          , dartists  = as
                          , dreleased = treleased
                          , dadded    = tcreated
                          , dcover    = tcover
                          , dfolder   = 999
                            }
        rs = getRelease <$> tis

      rs = case res of
        Left _ -> []
        Right t -> getReleases t
  putStrLn "-----------------Getting Favorite Albums from Tidal-----"
  F.for_ rs (\r -> print $
      show (FJ.dtitle r) <> show (FJ.dartists r))

  pure rs

data DToken = DToken Token UserName deriving (Show)
data DEnv = DEnv { token :: Token
                 , username :: UserName
                 , dclient :: ClientEnv
                 }


refreshLists :: DToken -> IO ( M.Map Text ( Vector Int ) )
refreshLists (DToken tok un) = do
  manager <- newManager tlsManagerSettings  -- defaultManagerSettings
  let denv :: DEnv
      denv = DEnv { token = tok
                  , username = un
                  , dclient = mkClientEnv manager ( BaseUrl Https "api.discogs.com" 443 [] )
                  }
-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs arf unique
-- NB: the JSON required to extract album id info ir different between them
  let readListAids :: WList -> IO ( Text, Vector Int )
      readListAids ( WList i t ) = do
        res <- runClientM ( getList i ( Just ( token denv ) ) ( Just "ClutterApp/0.1" ))
                          ( dclient denv )
        case res of
          Left err -> putStrLn $ "Error: " ++ show err
          Right ls -> -- pure ()
                      F.traverse_ print $ take 5 . wlitems $ ls
        let aids  = wlaid <$> V.fromList ( wlitems ( fromRight (WLItems []) res ))
        return ( t, aids )
-- get list and folder names and ids
  let queries :: DEnv -> ClientM ( WFolders, WLists )
      queries (DEnv tok un _) = do
        efs <- getFolders un ( Just tok ) ( Just "ClutterApp/0.1" )
        els <- getLists   un ( Just tok ) ( Just "ClutterApp/0.1" )
        return ( efs, els )
  res <- runClientM ( queries denv ) ( dclient denv )
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ( fs, ls ) -> do
      let ds = V.fromList ( folders fs )
      F.for_ ds print
      let ds = V.fromList ( lists ls )
      F.for_ ds print

  (t, aids) <- readListAids ( WList 540434 "Listened" )
  return ( M.singleton t aids )


