{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}

module FromWeb  ( refreshLists
                , DToken (..)
                ) where

-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as S8
-- import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.ByteString.Lazy as BL ( readFile, putStrLn )
import Data.Vector ( Vector )
import qualified Data.Vector as V (fromList, toList, take )
import qualified Data.Foldable as F ( for_  )
import Data.Either (fromRight)

-- import System.Process ( callCommand )

import Network.HTTP.Client ( defaultManagerSettings, newManager, httpLbs )
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
       "releases/249504"
       :> QueryParam "token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WTest
  :<|> "users"
       :> Capture "name" UserName
       :> "collection/folders"
       :> QueryParam "token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WFolders
  :<|> "users"
       :> Capture "name" UserName
       :> "lists"
       :> QueryParam "token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WLists
  -- :<|> "lists"
  --      :> Capture "listid" Int
  --      :> QueryParam "token" Token
  --      :> Header "User-Agent" UserAgent
  --      :> Get '[JSON] WLists

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
-- getList :: Int
--          -> Maybe Token
--          -> Maybe UserAgent
--          -> ClientM WList

discogsAPI :: Proxy DiscogsAPI
discogsAPI = Proxy

getTest :<|> getFolders :<|> getLists {- :<|> getList -}= client discogsAPI

newtype DToken = DToken Text deriving (Show)

queries :: Token -> ClientM ( WFolders, WLists )
queries tok = do
  efs <- getFolders "LATB" ( Just tok ) ( Just "ClutterApp/0.1" )
  els <- getLists   "LATB" ( Just tok ) ( Just "ClutterApp/0.1" )
  return ( efs, els )

refreshLists :: DToken -> IO ()
refreshLists ( DToken dt ) = do
  manager' <- newManager tlsManagerSettings  -- defaultManagerSettings
  let
      name = "LATB"
      path = "users/" ++ name
      cl :: ClientEnv
      cl = mkClientEnv manager' ( BaseUrl Https "api.discogs.com" 443 [] )
  res <- runClientM ( queries dt ) cl
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ( fs, ls ) -> do
      let ds = V.fromList ( folders fs )
      F.for_ ds print
      let ds = V.fromList ( lists ls )
      F.for_ ds print

  -- res' <- runClientM ( queries' dt ) cl
-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs arf unique
-- NB: the JSON required to extract album id info ir different between them
  -- let readListAids :: WLists -> IO ( Text, Vector Int )
  --     readListAids ( WLists i t ) = do
  --         let fn = "data/l" ++ show i ++ ".json"
  --         aids <- readDAids fn
  --         return ( t, aids )
  return ()

-- queries' :: Token -> ClientM ( WFolders, WLists )
-- queries' tok = do
--   return

-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs arf unique
-- NB: the JSON required to extract album id info ir different between them
-- readWAids :: Int -> Token -> ClientM ( Vector Int )
-- readWAids lid tok = do
        -- d <- getList lid ( Just tok ) ( Just "ClutterApp/0.1" )
        -- case d of
        --   Left err -> putStrLn err
        --   Right _ -> pure () -- print ds
        -- let ds = V.fromList $ fromRight [] (wlitems d)
        --     aids  = wlaid <$> ds
        -- return aids





-- curl "https://api.discogs.com/lists/540434" -H "Authorization: Discogs token=<token>"   > data/l540434-raw.json



