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

import Types ( Env (..), EnvR (..), SortOrder (..) )

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
          am <- liftIO ( readIORef (albumsR env) )
          let mAlbum = M.lookup aid am
          pure . RawHtml $ L.renderBS (renderAlbum mAlbum)

        serveAlbums :: Text -> Maybe Text -> Maybe Text -> Handler RawHtml
        serveAlbums listName msb mso = do
          aids' <- liftIO ( getList env env listName )
          am    <- liftIO ( readIORef (albumsR env) )
          lns   <- liftIO ( readIORef (listNamesR env) )
          sn    <- case msb of
                    Nothing -> liftIO ( readIORef (sortNameR env) )
                    Just sb -> do
                                _ <- writeIORef ( sortNameR env ) sb
                                pure sb
          so    <- case mso of
                    Nothing -> liftIO ( readIORef (sortOrderR env) )
                    Just sot ->  do
                                  let so = case sot of
                                          "Desc" -> Desc
                                          _      -> Asc
                                  _ <- writeIORef ( sortOrderR env ) so
                                  pure so

          let doSort = getSort env am sn
          let aids = doSort so aids'
          lm <- liftIO ( readIORef (listsR env) )
          lcs <- liftIO ( readIORef (locsR env) )
          di <- liftIO ( readIORef (discogsR env) )
          let envr = EnvR am lm lcs lns sn so di
          pure . RawHtml
                $ L.renderBS ( renderAlbums env envr listName aids )
        -- serveJSON :: Server API2
        -- serveJSON = do
        --   pure $ M.elems ( albums env )

        serveDiscogs :: Text -> Text -> Handler RawHtml
        serveDiscogs token username = do
          _   <- liftIO ( refreshEnv env token username )
          am  <- liftIO ( readIORef (albumsR env) )
          lm  <- liftIO ( readIORef (listsR env) )
          lcs <- liftIO ( readIORef (locsR env) )
          lns <- liftIO ( readIORef (listNamesR env ) )
          sn  <- liftIO ( readIORef (sortNameR env) )
          so  <- liftIO ( readIORef (sortOrderR env) )
          di  <- liftIO ( readIORef (discogsR env) )
          let ln = "Discogs"
          aids <- liftIO ( getList env env ln )
          let envr = EnvR am lm lcs lns sn so di
          pure . RawHtml $ L.renderBS ( renderAlbums env envr ln aids )

        serveTidal :: Text -> Text -> Handler RawHtml
        serveTidal token username = do
          _   <- liftIO ( refreshEnv env token username )
          am  <- liftIO ( readIORef (albumsR env) )
          lm  <- liftIO ( readIORef (listsR env) )
          lcs <- liftIO ( readIORef (locsR env) )
          lns <- liftIO ( readIORef (listNamesR env ) )
          sn  <- liftIO ( readIORef (sortNameR env) )
          so  <- liftIO ( readIORef (sortOrderR env) )
          di  <- liftIO ( readIORef (discogsR env) )
          let ln = "Tidal"
          aids <- liftIO ( getList env env ln )
          let envr = EnvR am lm lcs lns sn so di
          pure . RawHtml $ L.renderBS ( renderAlbums env envr ln aids )


-- type App = ReaderT Env IO
-- type AppM = ReaderT Env Handler
startApp :: IO ()
startApp = initEnv >>= ( run 8080 . app )
-- startApp = do
--   env <- initEnv
--   ( run 8080 . app ) env



app :: Env -> Application
app env = serve api $ server env

