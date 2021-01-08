{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}



module Lib
    ( startApp
    , app
    , initEnv
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified Data.Map.Strict as M

import qualified Data.ByteString.Lazy as BL
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))

import Env ( Env (..)
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
type API1 = "albums" :> Get '[HTML] RawHtml
-- type API2 = "albumj" :> Get '[JSON] [Album]
type API = API0 :<|> API1
  -- :<|> API2 
  :<|> Raw
api :: Proxy API
api = Proxy

server :: Env -> Server API
server env = serveAlbum :<|> serveAlbums
  -- :<|> serveJSON 
  :<|> serveDirectoryFileServer "static"
  where serveAlbum :: Int -> Handler RawHtml
        serveAlbum aid = do
          let mAlbum = M.lookup aid ( albums env )
          return $ RawHtml $ L.renderBS (renderAlbum mAlbum)
        serveAlbums :: Handler RawHtml
        serveAlbums = do
          return $ RawHtml $ L.renderBS (renderAlbums env)
        -- serveJSON :: Server API2
        -- serveJSON = do
        --   return $ M.elems ( albums env )

startApp :: IO ()
startApp = initEnv >>= ( run 8080 . app )

app :: Env -> Application
app env = serve api $ server env

