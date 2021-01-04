{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}



module Lib
    ( startApp
    , app, initEnv
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad.IO.Class ( liftIO )
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F
import Data.Maybe ( mapMaybe )
import qualified Data.Vector as V ( Vector (..), toList, fromList, singleton )
import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Data.Text (Text)
import Text.RawString.QQ

import qualified Data.Text as T
import FromJSON ( Album (..)
                , Tidal (..)
                , Discogs (..)
                , readDLists
                , readDFolders
                , albumMap
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
startApp = do
  env <- initEnv
  run 8080 ( serve api $ server env )

----------------------- Provider stuff

app = serve api $ server env
  where 
        a = Album 123123
                  "Test Title"
                  "Test Artists"
                  "2021"
                  "https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg"
                  "2021-01-01T01:23:01-07:00" "Pop" (\a -> "xxx")
        env :: Env
        env = Env { albums = M.singleton 1 a, aidAdded = V.singleton ( albumID a ) }

initEnv :: IO (Env)
initEnv = do
  let   a = Album 123123
                  "Test Title"
                  "Test Artists"
                  "2021"
                  "https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg"
                  "2021-01-01T01:23:01-07:00" "Pop" (\_ -> "xxx")

  let am0 = M.singleton 1 a
  am1 <- albumMap $ Discogs "data/dall.json"
  am2 <- albumMap $ Tidal "data/tall.json"
  let am = M.unions [ am0, am1, am2 ]

  vl <- readDLists
  vf <- readDFolders
  

  return $ Env { albums = am, aidAdded = snd vf } -- sortAdded am }


data Env = Env { albums :: M.Map Int Album, aidAdded :: V.Vector Int }
sortAdded :: (M.Map Int Album) -> V.Vector Int -- reverse chronological
sortAdded am = V.fromList $ fst <$> (sortBy ( \ (_,a) (_,b) -> comparing albumAdded b a ) $ M.toList am)
-- aidTitles :: (M.Map Int Album) -> V.Vector Int
-- aidTitles am = V.fromList $ fst <$> (sortBy ( \ (_,a) (_,b) -> comparing albumTitle a b ) $ M.toList am)
-- aidArtists :: (M.Map Int Album) -> V.Vector Int
-- aidArtists am = V.fromList $ fst <$> (sortBy ( \ (_,a) (_,b) -> comparing albumArtist a b ) $ M.toList am)

--------------------------Render Stuff

renderAlbum :: Maybe Album -> L.Html ()
renderAlbum mAlbum = L.html_ $ do
  renderHead "Album Page"
  L.body_ $ albumBody
  where
    albumBody = case mAlbum of
      Nothing -> L.div_ [L.class_ "login-message"] $ do
          L.p_ "Unknown Album, Sorry!"
          L.br_ []
          L.a_ [L.href_ "/albums"] "Please see all Albums"
      Just a -> L.div_ [L.class_ "data-deskgap-drag"] $ do
          L.div_ [L.class_ "cover-art"] $ do
            L.img_ [ L.src_ (albumCover a), L.width_ "160"
                   , L.height_ "160", L.alt_ "cover image." ]
          L.p_ $ L.toHtml ("Title: " ++ T.unpack ( albumTitle a ))
          L.p_ $ L.toHtml ("Artist: " ++ T.unpack ( albumArtist a ))
          L.p_ $ L.toHtml ("Year: " ++ T.unpack (albumReleased a))
          L.br_ []

renderAlbums :: Env -> L.Html ()
renderAlbums env = 
    -- L.doctype_ "html"
    L.html_ $ do
      renderHead "Clutter - Album Grid"
      L.body_ $ albumBody
      where
        albumBody = do
          -- L.h1_ "Test test test"
          L.div_ [L.class_ "data-deskgap-drag"] $ do
            L.div_ [L.class_ "row"] $ do
              renderTNs env $ ( aidAdded env )
              -- F.traverse_ renderAlbumTN env

renderTNs :: Env -> V.Vector Int -> L.Html ()
renderTNs env aids =
  F.traverse_ renderAlbumTN $ mapMaybe ( \ aid -> M.lookup aid ( albums env ) ) $ V.toList aids

renderAlbumTN :: Album -> L.Html ()
renderAlbumTN a =
        L.div_ [L.class_ "album-thumb"] $ do
          L.div_ [L.class_ "cover-art"] $ do
            -- L.a_ [L.href_ $ T.pack ("https://www.discogs.com/release/" ++ show (albumID a))] $ do
            -- L.a_ [L.href_ $ T.pack ("https://www.tidal.com/album/" ++ show (albumID a))] $ do
            L.a_ [L.href_ $ ((albumURL a) a)] $ do
              L.img_ [ L.src_ (albumCover a), L.alt_ "cover image." ]
          L.div_ [L.class_ "album-info"] $ do
            L.p_ [L.class_ "album-title"] $ do
              L.toHtml (T.unpack ( albumTitle a ))
            L.p_ [L.class_ "album-artist"] $ do
              L.toHtml (T.unpack ( albumArtist a ))


renderHead :: Text -> L.Html ()
renderHead title =
      L.head_ $ do
        L.meta_ [L.charset_ "utf-8"]
        L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1.0"]
        L.meta_ [L.httpEquiv_ "X-UA-Compatible", L.content_ "ie=edge"]
        L.link_ [L.rel_ "stylesheet", L.type_ "text/css", L.href_ "/styles.css"]
        L.style_ styleqq
        L.title_ "Clutter - Album Grid"

styleqq :: Text
styleqq = [r|
* {
  box-sizing: border-box;
}

|]

    -- a = Album 123123 "Test Title" "Test Artists" 2020 "https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg"
