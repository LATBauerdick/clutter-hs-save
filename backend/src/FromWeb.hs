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
import qualified Data.Map as M
import Data.Vector ( Vector )
import qualified Data.Vector as V (fromList, toList, take )
import qualified Data.Foldable as F ( for_  )
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
  :<|> "lists"
       :> Capture "listid" Int
       :> QueryParam "token" Token
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

data DToken = DToken Token UserName deriving (Show)

queries :: DToken -> ClientM ( WFolders, WLists )
queries (DToken tok un) = do
  efs <- getFolders un ( Just tok ) ( Just "ClutterApp/0.1" )
  els <- getLists   un ( Just tok ) ( Just "ClutterApp/0.1" )
  return ( efs, els )

data DEnv = DEnv { dtoken :: DToken
                 , dclient :: ClientEnv
                 }

refreshLists :: DToken -> IO ( M.Map Text ( Vector Int ) )
refreshLists dt = do
  manager' <- newManager tlsManagerSettings  -- defaultManagerSettings
  let denv :: DEnv
      denv = DEnv { dtoken = dt
                  , dclient = mkClientEnv manager' ( BaseUrl Https "api.discogs.com" 443 [] )
                  }
-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs arf unique
-- NB: the JSON required to extract album id info ir different between them
  let readListAids :: WList -> IO ( Text, Vector Int )
      readListAids ( WList i t ) = do
        res <- runClientM ( queries' (dtoken denv) i )  (dclient denv)
        case res of
          Left err -> putStrLn $ "Error: " ++ show err
          Right _ -> pure ()
          -- Right ( lis ) -> do
          --   let ds = V.fromList ( wlitems lis )
          --   F.for_ ds print
        let is = V.fromList $ wlitems ( fromRight (WLItems { wlitems = [] }) res )
            aids  = wlaid <$> is
        return ( t, aids )
-- get list and folder names and ids
  res <- runClientM ( queries (dtoken denv) ) (dclient denv)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right ( fs, ls ) -> do
      let ds = V.fromList ( folders fs )
      F.for_ ds print
      let ds = V.fromList ( lists ls )
      F.for_ ds print
  (t, aids) <- readListAids ( WList 540434 "Listened" )
  let lm :: M.Map Text ( Vector Int )
      lm = M.singleton "Listened" aids

--   res <- runClientM ( queries' (dtoken denv) 540434 )  (dclient denv)
--   case res of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right ( lis ) -> do
--       let ds = V.fromList ( wlitems lis )
--       F.for_ ds print
--   let is = V.fromList $ wlitems ( fromRight (WLItems { wlitems = [] }) res )
--       aids  = wlaid <$> is
--       lm :: M.Map Text ( Vector Int )
--       lm = M.singleton "Listened" aids

  return lm

queries' :: DToken -> Int -> ClientM ( WLItems )
queries' (DToken tok _) lid = do
  d <- getList lid ( Just tok ) ( Just "ClutterApp/0.1" )
  return d

-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs arf unique
-- NB: the JSON required to extract album id info ir different between them
-- readWAids :: Int -> Token -> ClientM ( Vector Int )
-- readWAids lid tok = do
--         d <- getList lid ( Just tok ) ( Just "ClutterApp/0.1" )
--         case d of
--           Left err -> putStrLn err
--           Right _ -> print d
--         let ds = V.fromList $ fromRight [] (wlitems d)
--             aids  = wlaid <$> ds
--         return aids





-- curl "https://api.discogs.com/lists/540434" -H "Authorization: Discogs token=<token>"   > data/l540434-raw.json



