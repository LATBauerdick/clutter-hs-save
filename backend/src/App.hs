{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module App
    ( startApp
    , app
    , initEnv
    ) where
import Relude

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified Data.Map.Strict as M

import qualified Data.ByteString.Lazy as BL
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))

import Env ( refreshEnv
           , initEnv
           )

import Types ( Env (..), SortOrder (..) )

import Render ( renderAlbum
              , renderAlbums
              )

data HTML = HTML

newtype RawHtml = RawHtml { unRaw :: BL.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

------------------ Servant Stuff

type API0 = "album"  :> Capture "aid" Int :> Get '[HTML] RawHtml
type API1 = "albums"
          :> Capture "list" Text
          :> QueryParam "sortBy" Text
          :> QueryParam "sortOrder" Text
          :> Get '[HTML] RawHtml
type API3 = "provider" :> "discogs" :> Capture "token" Text :> Capture "username" Text :> Get '[HTML] RawHtml
type API4 = "provider" :> "tidal" :> Capture "token" Text :> Capture "username" Text :> Get '[HTML] RawHtml
type API = API0
      :<|> API1
      :<|> API3
      :<|> API4
      :<|> Raw
api :: Proxy API
api = Proxy

server :: Env -> Server API
server env = serveAlbum
        :<|> serveAlbums
        :<|> serveDiscogs
        :<|> serveTidal
        :<|> serveDirectoryFileServer "static"
  where serveAlbum :: Int -> Handler RawHtml
        serveAlbum aid = do
          am <- liftIO ( readIORef (albums env) )
          let mAlbum = M.lookup aid am
          return $ RawHtml $ L.renderBS (renderAlbum mAlbum)

        serveAlbums :: Text -> Maybe Text -> Maybe Text -> Handler RawHtml
        serveAlbums list msb mso = do
          aids' <- liftIO ( getList env env list )
          am <- liftIO ( readIORef (albums env) )
          lns <- liftIO ( readIORef (listNames env) )
          sn <- case msb of
                 Nothing -> liftIO ( readIORef (sortName env) )
                 Just sb -> do
                              _ <- writeIORef ( sortName env ) sb
                              pure sb
          so <- case mso of
                 Nothing -> liftIO ( readIORef (sortOrder env) )
                 Just sot ->  do
                                let so = case sot of
                                          "Desc" -> Desc
                                          _      -> Asc
                                _ <- writeIORef ( sortOrder env ) so
                                pure so

          let doSort = getSort env am sn
          let aids = doSort so aids'
          lm <- liftIO ( readIORef (lists env) )
          return $ RawHtml
                $ L.renderBS ( renderAlbums env am lm aids lns list sn so )
        -- serveJSON :: Server API2
        -- serveJSON = do
        --   return $ M.elems ( albums env )

        serveDiscogs :: Text -> Text -> Handler RawHtml
        serveDiscogs token username = do
          _ <- liftIO ( refreshEnv env token username )
          am <- liftIO ( readIORef (albums env) )
          aids <- liftIO ( getList env env "Discogs" )
          lns <- liftIO ( readIORef (listNames env ) )
          lm <- liftIO ( readIORef (lists env) )
          return $ RawHtml $ L.renderBS ( renderAlbums env am lm aids lns "Discogs" "Default" Asc )

        serveTidal :: Text -> Text -> Handler RawHtml
        serveTidal token username = do
          _ <- liftIO ( refreshEnv env token username )
          am <- liftIO ( readIORef (albums env) )
          aids <- liftIO ( getList env env "Tidal" )
          lns <- liftIO ( readIORef (listNames env ) )
          lm <- liftIO ( readIORef (lists env) )
          return $ RawHtml $ L.renderBS ( renderAlbums env am lm aids lns "Tidal" "Default" Asc )


-- type App = ReaderT Env IO
-- type AppM = ReaderT Env Handler
startApp :: IO ()
startApp = initEnv >>= ( run 8080 . app )
-- startApp = do
--   env <- initEnv
--   ( run 8080 . app ) env



app :: Env -> Application
app env = serve api $ server env

