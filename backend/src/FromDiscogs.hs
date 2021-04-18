{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module FromDiscogs ( refreshLists
                   , readDiscogsReleases
                   , readDiscogsLists
                   , readListAids
                   , readDiscogsFolders
                   ) where
import Relude

import Types ( Release (..), DiscogsInfo (..) )

import qualified Data.Map as M
import Data.Vector ( Vector )
import qualified Data.Vector as V (fromList, empty )

import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )

import Data.Aeson ( (.:), (.:?), (.!=), FromJSON (..), withObject )
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
                         , notes :: [WNote]
                         } deriving (Show, Generic)
instance FromJSON WRelease where
  parseJSON = withObject "release" $ \ o -> do
    daid_      <- o .: "id"
    dadded_    <- o .: "date_added"
    fid_       <- o .: "folder_id"
    bi_        <- o .: "basic_information"
    notes_     <- o .:? "notes" .!= []
    return ( WRelease daid_ dadded_ fid_ bi_ notes_ )
data WBasicInfo = WBasicInfo { title :: !Text
                             , year :: Int
                             , cover_image :: !Text
                             , artists :: [WArtist]
                             , formats :: [WFormat]
                             } deriving (Show, Generic)
data WNote = WNote { field_id :: Int
                   , value :: !Text
                   } deriving (Show, Generic)
data WArtist = WArtist { name :: !Text
                       } deriving (Show, Generic)
data WFormat = WFormat { name :: !Text
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
-- instance FromJSON WRelease
instance FromJSON WBasicInfo
instance FromJSON WNote
instance FromJSON WArtist
instance FromJSON WFormat
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
       -- "releases/249504"
       -- :> Header "Authorization: Discogs token" Token
       -- :> Header "User-Agent" UserAgent
       -- :> Get '[JSON] WTest
-- GET releases
       "users"
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
  -- :<|> "users"
  --      :> Capture "name" UserName
  --      :> "collection" :> "folders"
  --      :> Capture "folder_id" Int
  --      :> "releases"
  --      :> QueryParam "token" Token
  --      -- :> Header "Authorization: Discogs token" Token
  --      :> Header "User-Agent" UserAgent
  --      :> Get '[JSON] WReleases'

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
-- getFolder :: UserName
--          -> Int
--          -> Maybe Token
--          -> Maybe UserAgent
--          -> ClientM WReleases'

discogsAPI :: Proxy DiscogsAPI
discogsAPI = Proxy

getReleases :<|> getFolders :<|> getLists :<|> getList = client discogsAPI

--
-- ToDo:
--   get list and folder names/ids first
--   then get all releases and create the folder lists 
--   directly from the folder id in the release record
--   lists should only be constructed lazily
--
readDiscogsReleases :: DiscogsInfo -> IO [Release]
readDiscogsReleases di = do
  m <- newManager tlsManagerSettings  -- defaultManagerSettings
  let DiscogsSession tok un = di
  let dc = mkClientEnv m discogsBaseUrl
      query :: ClientM [WRelease]
      query = do
        let getWr :: WReleases -> [WRelease]
            getWr wr = rs where
                        WReleases { pagination = WPagination { pages = _
                                                 , items = _
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
  res <- runClientM query dc
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let getR :: WRelease -> Release
      getR dr = r where
          WRelease { id = did
                   , date_added = da
                   , folder_id = dfolder_id
                   , basic_information =
                       WBasicInfo { title=dt
                                  , year=dyear
                                  , cover_image=dcov
                                  , artists=das
                                  , formats=dfs
                                  }
                   , notes = ns
                   } = dr
          as = (\ WArtist { name=n } -> n ) <$> das
          turl :: Maybe Text
          turl = case mapMaybe (\ WNote { field_id=i, value=v } -> if i /= 6 then Nothing else Just v) ns of
                  [a] -> Just a
                  _ -> Nothing
          fs = (\ WFormat { name=n } -> n ) <$> dfs
          r = Release  { daid      = did
                       , dtitle    = dt
                       , dartists  = as
                       , dreleased = show dyear
                       , dadded    = da
                       , dcover    = dcov
                       , dfolder   = dfolder_id
                       , dformat   = fs
                       , dtidalurl  = turl
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
  m <- newManager tlsManagerSettings  -- defaultManagerSettings
  let DiscogsSession tok un = di
  let dc = mkClientEnv m discogsBaseUrl
-- get list and folder names and ids
  putTextLn "-----------------Getting Lists from Discogs-----"
  let query :: ClientM WLists
      query = getLists un ( Just tok ) userAgent
  res <- runClientM query dc
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure  ()
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
  m <- newManager tlsManagerSettings
  let DiscogsSession tok un = di
      dc = mkClientEnv m discogsBaseUrl
-- get list and folder names and ids
  putTextLn "-----------------Getting Folders from Discogs-----"
  res <- runClientM ( getFolders un ( Just tok ) userAgent ) dc
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> do pure ()
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
  let DiscogsSession tok _ = di
  m <- newManager tlsManagerSettings
  let dc = mkClientEnv m discogsBaseUrl
  putTextLn $ "-----------------Getting List " <> show i <> " from Discogs-----"
  res <- runClientM ( getList i ( Just tok ) userAgent ) dc
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
                -- F.traverse_ print $ take 5 . wlitems $ ls
  let aids  = wlaid <$> V.fromList ( wlitems ( fromRight (WLItems []) res ))
  return aids
--
-- for a Discog folder_id, get the lists of album ids from Discogs
--
-- readFolderAids :: DiscogsInfo -> Int -> IO ( Vector Int )
-- readFolderAids di i = do
--   let DiscogsSession tok un = di
--   m <- newManager tlsManagerSettings
--   let dc = mkClientEnv m discogsBaseUrl
--   putTextLn $ "-----------------Getting Folder " <> show i <> " from Discogs-----"
--   res <- runClientM ( getFolder un i ( Just tok ) userAgent ) dc
--   case res of
--           Left err -> putTextLn $ "Error: " <> show err
--           Right _ -> pure ()
--   let getRs :: WReleases' -> [Int]
--       getRs r = is where
--               geti :: WRelease' ->Int
--               geti r = i
--                       where WRelease' {id=i} = r
--               WReleases' { releases=rs } = r
--               is = geti <$> rs
--   let is = case res of
--               Left _ -> []
--               Right r -> getRs r
--   return ( V.fromList is )
--
--
refreshLists :: DiscogsInfo -> IO ( Map Text (Int, Vector Int) )
refreshLists di = do
  let DiscogsSession tok un = di
  m <- newManager tlsManagerSettings  -- defaultManagerSettings
  let dc = mkClientEnv m discogsBaseUrl
  let query :: ClientM WLists
      query = getLists un (Just tok) userAgent
  putTextLn "-----------------Refreshing Lists from Discogs-----"
  res <- runClientM query dc
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let ls = case res of
        Left _ -> []
        Right wls -> lists wls
  let lm :: [ ( Text, (Int, Vector Int) ) ]
      lm = (\ WList {id=i, name=n} -> ( n, ( i, V.empty ))) <$> ls
  return $ M.fromList lm

