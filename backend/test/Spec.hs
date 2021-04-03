module Main (main) where
import Relude hiding (get)

import App ( app )
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
  t <- readFileText "data/tok.dat"
  let [t0, t1, t2, t3, t4] = words t
  let userId = fromMaybe 0 $ readMaybe ( toString t2 ) :: Int
      sessionId = t3
      countryCode = t4
      discogsToken = t0
      discogsUser = t1
  -- vta <- readAlbums $ Tidal $ TidalSession userId sessionId countryCode
  testEnv <- initEnv

  let spec :: Spec
      spec = with (return ( app testEnv )) $ do
        -- describe "GET /albums/Listened" $ do
        --   it "responds with 200" $ do
        --     get "/albums/Listened" `shouldRespondWith` 200

        describe "GET /provider/discogs/<discogs-token>/<discogs-user>" $ do
          it "responds with 200" $ do
            get (S8.pack ( "/provider/discogs/" ++ toString discogsToken ++ "/" ++ toString discogsUser )) `shouldRespondWith` 200

  hspec spec

