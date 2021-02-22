{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib ( app )
import Env ( Env (..), initEnv )
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.Text as T

import Provider ( Discogs (..)
                , readAlbums
                , Tidal (..)
                , TidalInfo (..)
                )

main :: IO ()
main = do
  t <- readFile "data/tok.dat"
  let userId = read ( words t !! 2 ) :: Int
      sessionId = T.pack $ words t !! 3
      countryCode = T.pack $ words t !! 4
  vta <- readAlbums $ Tidal $ TidalSession userId sessionId countryCode
  -- testEnv <- initEnv Nothing Nothing

  let spec :: Spec
      spec = with (return ( app testEnv )) $ do
        -- describe "GET /albums/Listened" $ do
        --   it "responds with 200" $ do
        --     get "/albums/Listened" `shouldRespondWith` 200

        describe "GET /provider/discogs/<discogs-token>/<discogs-user>" $ do
          it "responds with 200" $ do
            let discogsToken = head . words $ t
            let discogsUser = words t !! 1
            get (S8.pack ( "/provider/discogs/" ++ discogsToken ++ "/" ++ discogsUser )) `shouldRespondWith` 200

  hspec spec

