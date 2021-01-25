{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module FromWeb  ( refreshLists
                , DToken (..)
                ) where

-- import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as BL ( readFile )
import Data.Vector ( Vector )
import qualified Data.Vector as V (fromList, toList, take )
import qualified Data.Foldable as F ( for_  )
import Data.Either (fromRight)

import Network.HTTP.Client -- (defaultManagerSettings, newManager, httpLbs, get)
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Network.HTTP.Types.Status (statusCode)

import Web.Authenticate.OAuth
import Data.Aeson ( FromJSON, eitherDecode, Value )
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.Text as T ( unpack )
import GHC.Generics
import qualified Data.Yaml as Yaml

newtype DToken = DToken Text deriving (Show)

data WLists = WLists { pagination :: WPagination
                     , lists :: [WList]
                     } deriving (Show, Generic)

data WPagination = WPagination { pages :: Int
                               , items :: Int
                               } deriving (Show, Generic)
data WList = WList  { id :: Int
                    , name :: !Text
                    } deriving (Show, Generic)

data WFolders = WFolders { folders :: [WList]
                         } deriving (Show, Generic)

instance FromJSON WLists
instance FromJSON WFolders
instance FromJSON WList
instance FromJSON WPagination

wFolderNameIds :: String -> IO (Either String (Vector WList))
wFolderNameIds token = do
  let name = "LATB"
      reqURL = "https://api.discogs.com/users/" ++ name ++ "/folders?token=" ++ token
  request <- parseRequest reqURL
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  print $ responseBody response

  putStrLn "------ Trying read folders from file instead"
  d <- (eitherDecode <$> BL.readFile "data/folders-raw.json") :: IO (Either String WFolders)
  case d of
    Left err -> putStrLn err
    Right _ -> pure ()
  let ds = V.fromList ( folders ( fromRight ( WFolders { folders = [] } ) d ))
  return $ Right ds

wListNameIds :: String -> IO (Either String (Vector WList))
wListNameIds token = do
  let name = "LATB"
      reqURL = "https://api.discogs.com/users/" ++ name ++ "/lists?token=" ++ token
      -- reqURL = "https://httpbin.org/get"
  request <- parseRequest reqURL
  --     req = addRequestHeader "Authorization" ts
  --         $ req'
  manager <- newManager tlsManagerSettings

  putStrLn $ "Doing httpLBS with " ++ reqURL
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++
               show (statusCode $ responseStatus response)
  print $ responseBody response

  putStrLn "------ Trying read from file instead"
  d <- (eitherDecode <$> BL.readFile "data/lists-raw.json") :: IO (Either String WLists)
  case d of
    Left err -> putStrLn err
    Right _ -> pure () -- print d
  let ds = V.fromList ( lists ( fromRight ( WLists { lists = [] } ) d ))
  -- F.for_ ds print
  return $ Right ds
  -- return $ Left "Does not work yet"

refreshLists :: DToken -> IO ()
refreshLists ( DToken dt ) = do
  els <- wListNameIds $ T.unpack dt
  case els of
   Left err -> putStrLn err
   Right ls  -> mapM_ print $ V.take 5 ls
  efs <- wFolderNameIds $ T.unpack dt
  case efs of
   Left err -> putStrLn err
   Right fs  -> mapM_ print $ V.take 5 fs

