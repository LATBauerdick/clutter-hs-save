{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Render
  ( renderAlbum,
    renderAlbums,
  )
where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V (toList)
import Env (Env (..))
import qualified Lucid as L
import Relude
import Text.RawString.QQ
import Types (Album (..), TagFolder (..))

renderAlbum :: Maybe Album -> L.Html ()
renderAlbum mAlbum = L.html_ $ do
  renderHead "Album Page"
  L.body_ albumBody
  where
    albumBody = case mAlbum of
      Nothing -> L.div_ [L.class_ "login-message"] $ do
        L.p_ "Unknown Album, Sorry!"
        L.br_ []
        L.a_ [L.href_ "/albums"] "Please see all Albums"
      Just a -> L.div_ [L.class_ "data-deskgap-drag"] $ do
        L.div_ [L.class_ "cover-container"] $ do
          L.img_
            [ L.src_ (albumCover a),
              L.alt_ "cover image",
              L.class_ "cover-image"
            ]
          L.div_ [L.class_ "cover-overlay"] "Overlay Here"
        L.p_ $ L.toHtml ("Title: " <> albumTitle a)
        L.p_ $ L.toHtml ("Artist: " <> albumArtist a)
        L.p_ $ L.toHtml ("Year: " <> albumReleased a)
        L.br_ []

renderAlbums :: Env -> Map Int Album -> Vector Int -> Vector Text -> Text -> Text -> L.Html ()
renderAlbums env am aids lns ln sn =
  -- L.doctype_ "html"
  L.html_ $ do
    renderHead $ "Albums - " <> ln
    L.body_ albumBody
  where
    albumBody = do
      renderLeftMenu env lns ln sn
      -- grid of Albums
      L.div_ [L.class_ "albums"] $ do
        L.div_ [L.class_ "row"] $ do
          renderTNs am aids

renderLeftMenu :: Env -> Vector Text -> Text -> Text -> L.Html ()
renderLeftMenu env lns ln sn =
  L.ul_ $ do
    L.li_ $ L.a_ [L.class_ "active", L.href_ (url env <> "albums/All")] "Home"
    L.li_ [L.class_ "dropdown"] $ do
      L.a_ [L.class_ "dropbtn", L.href_ "javascript:void(0)"] $do
        L.toHtml ("List " <> ln <> " ")
        L.i_ [ L.class_ "fa fa-caret-down" ] ""
      L.div_ [L.class_ "dropdown-content"] $ do
        F.traverse_ addLink lns

    L.li_ [L.class_ "dropdown"] $ do
      L.a_ [L.class_ "dropbtn", L.href_ "javascript:void(0)"] $ do
        L.toHtml $ "Sort by " <> sn <> " "
        L.i_ [ L.class_ "fa fa-caret-down" ] ""
      L.div_ [L.class_ "dropdown-content"] $ do
        F.traverse_ addLink (sorts env)

    L.li_ $ L.a_ [L.href_ (url env)] "index"
  where
    addLink :: Text -> L.Html ()
    addLink t =
      L.a_ [L.href_ (url env <> "albums/" <> t)] $ do
        L.toHtml t

renderTNs :: Map Int Album -> Vector Int -> L.Html ()
renderTNs am aids =
  F.traverse_ renderAlbumTN $ mapMaybe (`M.lookup` am) (V.toList aids)

renderAlbumTN :: Album -> L.Html ()
renderAlbumTN a =
  L.div_ [L.class_ "album-thumb"] $ do
    L.div_ [L.class_ "cover-container"] $ do
      L.a_ [L.href_ (albumURL a a)] $ do
        L.img_
          [ L.src_ (albumCover a),
            L.alt_ "cover image.",
            L.class_ "cover-image"
          ]
        L.div_ [L.class_ "cover-overlay"] $ do
          if albumFolder a /= fromEnum TTidal then
              L.div_ [L.class_ "cover-obackground"] $ do
              L.img_ [ L.src_ "/discogs-icon.png", L.alt_ "D", L.class_ "cover-oimage" ]
            else ""
          case albumTidal a of
            Nothing -> ""
            Just turl -> L.div_ [L.class_ "cover-obackground1"] $ do
              L.a_ [L.href_ turl] $ do
                L.img_ [L.src_ "/tidal-icon.png", L.alt_ "T", L.class_ "cover-oimage"]
    L.div_ [L.class_ "album-info"] $ do
      L.p_ [L.class_ "album-title"] $ do
        L.toHtml (albumTitle a)
      L.p_ [L.class_ "album-artist"] $ do
        L.toHtml (albumArtist a)

renderHead :: Text -> L.Html ()
renderHead t =
  L.head_ $ do
    L.title_ $ L.toHtml t
    L.meta_ [L.charset_ "utf-8"]
    L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1.0"]
    L.meta_ [L.httpEquiv_ "X-UA-Compatible", L.content_ "ie=edge"]
    L.link_
      [ L.rel_ "stylesheet",
        L.type_ "text/css",
        L.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
      ]
    L.style_ styleqq

styleqq :: Text
styleqq =
  [r|

@font-face {
   font-family: sansation;
   src: url(sansation_light.woff);
}

body {
   font-family: "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif; 
   font-weight: 300;
}

p {
  margin: 0 0 0 0;
}

ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
  width: 12%;
  background-color: #f1f1f1;
  height: 100%; /* Full height */
  position: fixed; /* Make it stick, even on scroll */
  overflow: auto; /* Enable scrolling if the sidenav has too much content */
}

li a {
  display: block;
  color: #000;
  padding: 8px 16px;
  text-decoration: none;
}

/* Change the link color on hover */
li a:hover {
  background-color: #555;
  color: white;
}

.active {
  background-color: #333;
  color: white;
}

.dropbtn {
  background-color: #ff7000 /* #4CAF50*/;
  color: white;
  padding: 16px;
  font-size: 16px;
  border: none;
  cursor: pointer;
}

.dropdown {
  position: relative;
  display: inline-block;
}

.dropdown-content {
  display: none;
  position: absolute;
  background-color: #f9f9f9;
  min-width: 160px;
  box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
  z-index: 1;
}

.dropdown-content a {
  color: black;
  padding: 12px 16px;
  text-decoration: none;
  display: block;
}

.dropdown-content a:hover {background-color: #f9c87c /* #f1f1f1 */}

.dropdown:hover .dropdown-content {
  display: block;
}

.dropdown:hover .dropbtn {
  background-color: #f97432 /* #3e8e41 */;
}


* {
  box-sizing: border-box;
}

img {
/*  border: 1px solid #ddd; */
  border-radius: 4px;
  padding: 5px;
}

img:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

.album-thumb {
  padding: 0px 1px 10px;
}

.album-info {
  width: 210px;
}

p.album-title {
  white-space: nowrap; 
  /* border: 1px solid #ddd; */
  overflow: hidden;
  text-overflow: ellipsis;
  font-family: helvetica;
  font-size: 14px;
  margin: 2px 0 0 0;
}
p.album-artist {
  white-space: nowrap; 
  /* border: 1px solid #ddd; */
  overflow: hidden;
  text-overflow: ellipsis;
  font-family: helvetica;
  font-size: 11px;
  margin: 4px 0 0 0;
}

.albums {
  margin-left:12%;
  padding:1px 16px;
/*  height:1000px; */
 }

.row {
  display: flex;
  justify-content: space-evenly;
  flex-wrap: wrap;
  padding: 0 1px;
}

/* Container needed to position the overlay. Adjust the width as needed */
.cover-container {
  width:  205px;
  height: 205px;
  position: relative;
}
.cover-container:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

/* Make the image to responsive */
.cover-image {
  display: block;
  width: 100%;
  height: 100%;
  border-radius: 4px;
  padding: 5px;
  position: absolute;
  top: 0;
  left: 0;
}
.cover-image:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

/* The overlay effect - lays on top of the container and over the image */
.cover-overlay {
  width: 100%;
  height: 100%;
  position: absolute;
  top: 0;
  left: 0;
}

.cover-obackground {
  width: 24px;
  height: 24px;
  padding: 0px;
  border-radius: 4px;
  position: absolute;
  right: 7;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}
.cover-oimage {
  display: block;
  width: 24px;
  height: 24px;
  padding: 0px;
}

.cover-obackground1 {
  width: 24px;
  height: 24px;
  padding: 0px;
  border-radius: 4px;
  position: absolute;
  left: 7;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}
/* When you mouse over the container, fade in the overlay title */
/*.cover-container:hover .cover-overlay {
  opacity: 1;
}
*/

|]
