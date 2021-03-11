{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module FromDiscogs ( refreshLists
                   , readDiscogsReleases
                   , readDiscogsLists
                   , readListAids
                   , readDiscogsFolders
                   , DiscogsInfo (..)
                   ) where
import Relude

import qualified FromJSON as FJ ( Release (..) )

import qualified Data.Map as M
import Data.Vector ( Vector )
import qualified Data.Vector as V (fromList, empty )

import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )

import Data.Aeson ( (.:), FromJSON (..), withObject )
import GHC.Generics ()

-- import Control.Applicative
-- import Control.Monad
-- import Control.Monad.IO.Class

-- import Data.Proxy
import Servant
-- import Servant.API
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

newtype WFolders = WFolders { folders :: [WList]
                         } deriving (Show, Generic)

data WReleases = WReleases { pagination :: WPagination
                           , releases :: [WRelease]
                           } deriving (Show, Generic)
data WRelease = WRelease { id :: Int
                         , date_added :: !Text
                         , folder_id :: Int
                         , basic_information :: WBasicInfo
                         } deriving (Show, Generic)
data WBasicInfo = WBasicInfo { title :: !Text
                             , year :: Int
                             , cover_image :: !Text
                             , artists :: [WArtist]
                             } deriving (Show, Generic)
data WArtist = WArtist { name :: !Text
                       } deriving (Show, Generic)
data WReleases' = WReleases' { pagination :: WPagination
                             , releases :: [WRelease']
                             } deriving (Show, Generic)
data WRelease' = WRelease' { id :: Int
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
instance FromJSON WReleases
instance FromJSON WRelease
instance FromJSON WBasicInfo
instance FromJSON WArtist
instance FromJSON WReleases'
instance FromJSON WRelease'

type Token = Text
type UserName = Text
type UserAgent = Text

userAgent :: Maybe Text
userAgent = Just "ClutterApp/0.1 +http://bauerdick.org/clutter"
discogsBaseUrl :: BaseUrl
discogsBaseUrl =  BaseUrl Https "api.discogs.com" 443 []


type DiscogsAPI =
-- GET release
       "releases/249504"
       :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WTest

--curl "https://api.discogs.com/users/LATB/collection/folders/0/releases?page=1&per_page=500" -H "Authorization: Discogs token=<token>" > data/draw1.json
-- GET releases
  :<|> "users"
       :> Capture "name" UserName
       :> "collection" :> "folders"
       :> Capture "folder_id" Int
       :> "releases"
       -- :> QueryParam "sort" Text
       -- :> QueryParam "sort_order" Text
       :> QueryParam "page" Int
       :> QueryParam "per_page" Int
       :> QueryParam "token" Token
       -- :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WReleases
-- GET folders
  :<|> "users"
       :> Capture "name" UserName
       :> "collection" :> "folders"
       :> QueryParam "token" Token
       -- :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WFolders
-- GET lists
  :<|> "users"
       :> Capture "name" UserName
       :> "lists"
       :> QueryParam "token" Token
       -- :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WLists
-- Get list items
  :<|> "lists"
       :> Capture "listid" Int
       :> QueryParam "token" Token
       -- :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WLItems
-- Get Folder items
  :<|> "users"
       :> Capture "name" UserName
       :> "collection" :> "folders"
       :> Capture "folder_id" Int
       :> "releases"
       :> QueryParam "token" Token
       -- :> Header "Authorization: Discogs token" Token
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WReleases'

getTest ::  Maybe Token
           -> Maybe UserAgent
           -> ClientM WTest
getReleases :: UserName
           -> Int
           -- -> Maybe Text
           -- -> Maybe Text
           -> Maybe Int
           -> Maybe Int
           -> Maybe Token
           -> Maybe UserAgent
           -> ClientM WReleases
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
getFolder :: UserName
         -> Int
         -> Maybe Token
         -> Maybe UserAgent
         -> ClientM WReleases'

discogsAPI :: Proxy DiscogsAPI
discogsAPI = Proxy

getTest :<|> getReleases :<|> getFolders :<|> getLists :<|> getList :<|> getFolder = client discogsAPI

data DEnv = DEnv { token :: Token
                 , username :: UserName
                 , dclient :: ClientEnv
                 }

data DiscogsInfo = DiscogsFile FilePath | DiscogsSession Text Text
  deriving Show
--
-- ToDo:
--   get list and folder names/ids first
--   then get all releases and create the folder lists 
--   directly from the folder id in the release record
--   lists should only be constructed lazily
--
readDiscogsReleases :: DiscogsInfo -> IO [FJ.Release]
readDiscogsReleases di = do
  manager <- newManager tlsManagerSettings  -- defaultManagerSettings
  let DiscogsSession tok un = di
  let denv :: DEnv
      denv = DEnv { token = tok
                  , username = un
                  , dclient = mkClientEnv manager discogsBaseUrl
                  }
      query :: ClientM [WRelease]
      query = do
        let getWr :: WReleases -> [WRelease]
            getWr wr = rs where
                        WReleases { pagination = WPagination { pages = np
                                                 , items = n
                                                 }
                                  , releases = rs
                                  } = wr
        r0 <- getReleases un 0 (Just 1) (Just 500) ( Just tok )  userAgent
        let rs0 = getWr r0
        r1 <- getReleases un 0 (Just 2) (Just 500) ( Just tok )  userAgent
        let rs1 = getWr r1
        r2 <- getReleases un 0 (Just 3) (Just 500) ( Just tok )  userAgent
        let rs2 = getWr r2
        return $ rs0 <> rs1 <> rs2

  putTextLn "-----------------Getting Collection from Discogs-----"
  res <- runClientM query ( dclient denv )
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let getR :: WRelease -> FJ.Release
      getR dr = r where
          WRelease { id = did
                   , date_added = dadded
                   , folder_id = dfolder_id
                   , basic_information =
                       WBasicInfo { title=dtitle
                                  , year=dyear
                                  , cover_image=dcover
                                  , artists=dartists
                                  }
                   } = dr
          as = (\ WArtist { name=n } -> n ) <$> dartists
          r = FJ.Release  { daid      = did
                          , dtitle    = dtitle
                          , dartists  = as
                          , dreleased = show dyear
                          , dadded    = dadded
                          , dcover    = dcover
                          , dfolder   = dfolder_id
                            }

  let rs = case res of
        Left _ -> []
        Right d -> getR <$> d
  pure rs
--
--
--
readDiscogsLists :: DiscogsInfo -> IO ( Map Text ( Int, Vector Int ) )
readDiscogsLists di = do
  manager <- newManager tlsManagerSettings  -- defaultManagerSettings
  let DiscogsSession tok un = di
  let denv :: DEnv
      denv = DEnv { token = tok
                  , username = un
                  , dclient = mkClientEnv manager discogsBaseUrl
                  }
-- get list and folder names and ids
  putTextLn "-----------------Getting Lists from Discogs-----"
  let query :: ClientM WLists
      query = getLists (username denv) ( Just (token denv) ) userAgent
  res <- runClientM query ( dclient denv )
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right ls -> do pure () -- F.for_ (lists ls) print
  let ls = case res of
        Left _ -> []
        Right wls -> lists wls

  let lm :: [ ( Text, (Int, Vector Int) ) ]
      lm = (\ WList {id=i, name=n} -> ( n, ( i, V.empty ))) <$> ls
  return $ M.fromList lm
--
--
--
readDiscogsFolders :: DiscogsInfo -> IO ( Map Text Int )
readDiscogsFolders di = do
  manager <- newManager tlsManagerSettings
  let DiscogsSession tok un = di
      dclient = mkClientEnv manager discogsBaseUrl
-- get list and folder names and ids
  putTextLn "-----------------Getting Folders from Discogs-----"
  res <- runClientM ( getFolders un ( Just tok ) userAgent ) dclient
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right fs -> do pure () -- F.for_ (folders fs) print
  let fs :: [WList]
      fs = case res of
        Left _ -> []
        Right wfs -> folders wfs
  let fm :: [ ( Text, Int ) ]
      fm = (\ WList {id=i, name=n} -> ( n , i )) <$> fs
  return $ M.fromList fm

-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs arf unique
-- NB: the JSON required to extract album id info ir different between them
readListAids :: DiscogsInfo -> Int -> IO ( Vector Int )
readListAids di i = do
  let DiscogsSession tok un = di
  manager <- newManager tlsManagerSettings
  let dclient = mkClientEnv manager discogsBaseUrl
  putTextLn $ "-----------------Getting List " <> show i <> " from Discogs-----"
  res <- runClientM ( getList i ( Just tok ) userAgent ) dclient
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right ls -> pure ()
                -- F.traverse_ print $ take 5 . wlitems $ ls
  let aids  = wlaid <$> V.fromList ( wlitems ( fromRight (WLItems []) res ))
  return aids
--
-- for a Discog folder_id, get the lists of album ids from Discogs
--
readFolderAids :: DiscogsInfo -> Int -> IO ( Vector Int )
readFolderAids di i = do
  let DiscogsSession tok un = di
  manager <- newManager tlsManagerSettings
  let dclient = mkClientEnv manager discogsBaseUrl
  putTextLn $ "-----------------Getting Folder " <> show i <> " from Discogs-----"
  res <- runClientM ( getFolder un i ( Just tok ) userAgent ) dclient
  case res of
          Left err -> putTextLn $ "Error: " <> show err
          Right ls -> pure ()
  let getRs :: WReleases' -> [Int]
      getRs r = is where
              geti :: WRelease' ->Int
              geti r = i
                      where WRelease' {id=i} = r
              WReleases' { releases=rs } = r
              is = geti <$> rs
  let is = case res of
              Left _ -> []
              Right r -> getRs r
  return ( V.fromList is )
--
--
refreshLists :: DiscogsInfo -> IO ( Map Text (Int, Vector Int) )
refreshLists di = do
  let DiscogsSession tok un = di
  manager <- newManager tlsManagerSettings  -- defaultManagerSettings
  let denv :: DEnv
      denv = DEnv { token = tok
                  , username = un
                  , dclient = mkClientEnv manager discogsBaseUrl
                  }
-- get list and folder names and ids
  -- let queries :: DEnv -> ClientM ( WFolders, WLists )
  --     queries (DEnv tok un _) = do
  --       efs <- getFolders un ( Just tok ) userAgent
  --       els <- getLists   un ( Just tok ) userAgent
  --       return ( efs, els )
  let query :: ClientM WLists
      query = getLists (username denv) ( Just (token denv) ) userAgent
  putTextLn "-----------------Refreshing Lists from Discogs-----"
  res <- runClientM query ( dclient denv )
  -- res <- runClientM ( queries denv ) ( dclient denv )
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right ls -> pure ()
  let ls = case res of
        Left _ -> []
        Right wls -> lists wls
  let lm :: [ ( Text, (Int, Vector Int) ) ]
      lm = (\ WList {id=i, name=n} -> ( n, ( i, V.empty ))) <$> ls
  return $ M.fromList lm

  -- return ( M.singleton "Listened" undefined )
