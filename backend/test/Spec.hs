{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib ( app )
import Env ( Env (..), testEnv, initEnv )
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Provider ( Discogs (..), readAlbums )

main :: IO ()
main = do
  a <- readAlbums $ Discogs "data/tall.json"
  _ <- initEnv Nothing Nothing

  hspec spec

spec :: Spec
spec = with (return ( app testEnv )) $ do
    describe "GET /albums" $ do
        it "responds with 200" $ do
            get "/albums" `shouldRespondWith` 200
        it "responds with [Albums]" $ do
            let users = "<html><head><meta charset=\"utf-8\"><meta content=\"width=device-width, initial-scale=1.0\" name=\"viewport\"><meta content=\"ie=edge\" http-equiv=\"X-UA-Compatible\"><link href=\"/styles.css\" type=\"text/css\" rel=\"stylesheet\"><style>\n* {\n  box-sizing: border-box;\n}\n\n</style><title>Clutter - Album Grid</title></head><body><div class=\"data-deskgap-drag\"><div class=\"row\"><div class=\"album-thumb\"><div class=\"cover-art\"><a href=\"https://www.discogs.com/release/123123\"><img src=\"https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg\" alt=\"cover image.\"></a></div><div class=\"album-info\"><p class=\"album-title\">Test Title</p><p class=\"album-artist\">Test Artists</p></div></div></div></div></body></html>"
            get "/albums" `shouldRespondWith` users
    describe "GET /album/1" $ do
        it "responds with 200" $ do
            get "/album/1" `shouldRespondWith` 200
        -- it "responds with a album" $ do
        --     get "/album/1" `shouldRespondWith` "test not yet implementet"

