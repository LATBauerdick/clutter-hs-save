{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module FromTidal ( readTidalReleases
                 , TidalInfo (..)
                 ) where
import Relude

import qualified FromJSON as FJ ( Release (..) )

import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )

import Data.Aeson ( (.:), (.:?), (.!=), FromJSON (..), withObject )
-- import GHC.Generics

-- import Data.Proxy
import Servant
-- import Servant.API
import Servant.Client

data WTidal = WTidal
                { limit :: Int
                , totalNumberOfItems :: Int
                , items :: [WTItem]
                } deriving (Show, Generic)
instance FromJSON WTidal
data WTItem = WTItem
                { created :: !Text
                , item :: WTIItem
                } deriving (Show, Generic)
instance FromJSON WTItem
data WTIItem = WTIItem
                { id :: Int
                , title :: !Text
                , releaseDate :: !Text
                , url :: !Text
                , cover :: !Text
                , artists :: [WTArtist]
                } deriving (Show, Generic)
instance FromJSON WTIItem where
  parseJSON = withObject "wtiitem" $ \ o -> do
    id_           <- o .: "id"
    title_        <- o .: "title"
    releaseDate_  <- o .: "releaseDate"
    url_          <- o .: "url"
    cover_        <- o .:? "cover" .!= ""
    artists_      <- o .: "artists"
    return $ WTIItem id_ title_ releaseDate_ url_ cover_ artists_

data WTArtist = WTArtist
                { id :: Int
                , name :: !Text
                } deriving (Show, Generic)
instance FromJSON WTArtist


type UserAgent = Text
type TidalUserId = Int
type TidalSessionId = Text
type TidalCountryCode = Text
type TidalLimit = Int
type TidalOffset = Int
type TidalAPI =
-- GET Tidal favorites
-- https://api.tidalhifi.com/v1/users/<userId>/favorites/albums?sessionId=<session-id>&countryCode=US&limit=2999&offset=0
       "users"
       :> Capture "uid" TidalUserId
       :> "favorites" :> "albums"
       :> QueryParam "sessionId" TidalSessionId
       :> QueryParam "countryCode" TidalCountryCode
       :> QueryParam "limit" TidalLimit
       :> QueryParam "offset" TidalOffset
       :> Header "User-Agent" UserAgent
       :> Get '[JSON] WTidal
tidalAPI :: Proxy TidalAPI
tidalAPI = Proxy
getTidal :: TidalUserId
         -> Maybe TidalSessionId
         -> Maybe TidalCountryCode
         -> Maybe TidalLimit
         -> Maybe TidalOffset
         -> Maybe UserAgent
         -> ClientM WTidal
getTidal = client tidalAPI

data TEnv = TEnv { userId :: TidalUserId
                 , sessionId :: TidalSessionId
                 , countryCode :: TidalCountryCode
                 , tlimit :: TidalLimit
                 , toffset :: TidalOffset
                 , tclient :: ClientEnv
                 }
data TidalInfo = TidalFile FilePath | TidalSession Int Text Text
readTidalReleases :: TidalInfo -> IO [FJ.Release]
readTidalReleases tinf = do
  m <- newManager tlsManagerSettings  -- defaultManagerSettings
  let TidalSession uid sid cc = tinf
      tenv :: TEnv
      tenv = TEnv { userId = uid
                  , sessionId = sid
                  , countryCode = cc
                  , tlimit = 2999 -- 5
                  , toffset = 0 -- 1563 -- error at 1565
                  , tclient = mkClientEnv m ( BaseUrl Https "api.tidalhifi.com" 443 "v1" )
                  }
      tquery :: ClientM WTidal
      tquery  = getTidal (userId tenv)
                           ( Just (sessionId tenv) )
                           ( Just (countryCode tenv) )
                           ( Just (tlimit tenv) )
                           ( Just (toffset tenv) )
                           ( Just "ClutterApp/0.1" )
  putTextLn "-----------------Getting Favorite Albums from Tidal-----"
  res <- runClientM tquery ( tclient tenv )
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let getReleases :: WTidal -> [FJ.Release]
      getReleases t = getRelease <$> tis where
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
                          , dfolder   = 2
                          }

      rs = case res of
        Left _ -> []
        Right t -> getReleases t
  -- F.for_ (take 5 rs) (\r -> print $ show (FJ.dtitle r) <> show (FJ.dartists r))

  pure rs

