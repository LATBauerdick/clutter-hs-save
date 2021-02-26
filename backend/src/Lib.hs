{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Lib
    ( startApp
    , app
    , initEnv
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad.IO.Class ( liftIO )

import qualified Data.Map.Strict as M
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))

import qualified Data.Vector as V ( Vector, empty, null, length, take, singleton )

import Env ( Env (..)
           , refreshEnv
           , initEnv
           )

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
-- $(deriveJSON defaultOptions ''Album)

type API0 = "album"  :> Capture "aid" Int :> Get '[HTML] RawHtml
type API1 = "albums" :> Capture "list" Text :> Get '[HTML] RawHtml
-- type API2 = "albumj" :> Get '[JSON] [Album]
type API3 = "provider" :> "discogs" :> Capture "token" Text :> Capture "username" Text :> Get '[HTML] RawHtml
type API4 = "provider" :> "tidal" :> Capture "token" Text :> Capture "username" Text :> Get '[HTML] RawHtml
type API = API0 
      :<|> API1
   -- :<|> API2
      :<|> API3
      :<|> API4
      :<|> Raw
api :: Proxy API
api = Proxy

server :: Env -> Server API
server env = serveAlbum :<|> serveAlbums
  -- :<|> serveJSON
  :<|> serveDiscogs
  :<|> serveTidal
  :<|> serveDirectoryFileServer "static"
  where serveAlbum :: Int -> Handler RawHtml
        serveAlbum aid = do
          let mAlbum = M.lookup aid ( albums env )
          return $ RawHtml $ L.renderBS (renderAlbum mAlbum)

        serveAlbums :: Text -> Handler RawHtml
        serveAlbums list = do
          aids <- liftIO ( getList env list )
          let sn :: Text; sn = "Default" -- sort name should becom optional QueryParam
          let sort = getSort env sn
          return $ RawHtml
                $ L.renderBS ( renderAlbums env (sort aids) list sn )
        -- serveJSON :: Server API2
        -- serveJSON = do
        --   return $ M.elems ( albums env )

        serveDiscogs :: Text -> Text -> Handler RawHtml
        serveDiscogs token username = do
          _ <- liftIO ( refreshEnv env token username )
          return $ RawHtml $ L.renderBS ( renderAlbums env V.empty "Listened" "Default" )

        serveTidal :: Text -> Text -> Handler RawHtml
        serveTidal token username = do
          _ <- liftIO ( refreshEnv env token username )
          return $ RawHtml $ L.renderBS ( renderAlbums env V.empty "Listened" "Default" )

startApp :: IO ()
startApp = initEnv Nothing Nothing >>= ( run 8080 . app )

app :: Env -> Application
app env = serve api $ server env

