{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Provider ( readListAids
                , readAlbums
                , readLists
                , readFolders
                , readFolderAids
                , rereadLists
                , atest
                ) where
import Relude

import qualified FromJSON as FJ ( readReleases
                                , readLists
                                , readFolders
                                )
import qualified FromTidal as FT ( readTidalReleases )
import qualified FromDiscogs as FD ( readDiscogsReleases
                                   , readDiscogsLists
                                   , readListAids
                                   , readDiscogsFolders
                                   , rereadLists
                                   )

import Types ( Album (..),  Release (..)
             , Tidal (..), TidalInfo (..), getTidal
             , Discogs (..), DiscogsInfo (..), getDiscogs
             , TagFolder (..)  )
-- import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Map.Strict as M
import Data.Vector ( Vector )
import qualified Data.Vector as V

import qualified Data.Text as T

atest :: [ Album ]
atest  = [ Album 161314 "Mezzanine" "Massive Attack" "2001" "161314.jpg" "2018-01-01T18:01:42-08:00" 1349997 (const "https://www.tidal.com/album/161314") "Vinyl" Nothing Nothing 0 0
         , Album 5253301 "Beethoven - Symphonie Nr. 3 »Eroica« & 4" "Herbert von Karajan" "1992" "5253301.jpg" "2017-09-17T20:57:52-07:00" 1351871  (const "https://www.tidal.com/album/5253301") "Vinyl" Nothing Nothing 0 0
         ]

class Provider p where
  readAlbums :: p -> IO (Vector Album)
  readLists :: p ->  IO ( Map Text (Int, Vector Int ) )

instance Provider Tidal where
  readLists _ = error "Bug: Provider Tidal has no lists"
  readAlbums p = do
    let
        toCoverURL r = T.concat [
              T.pack "https://resources.tidal.com/images/"
            , T.intercalate "/" $ T.splitOn "-" (dcover r)
            , T.pack "/320x320.jpg" ]
        getAlbumURL :: Album -> Text
        getAlbumURL a = T.pack $
            "https://listen.tidal.com/album/" ++ show ( albumID a )
        toAlbum r = Album (daid r)
                          (dtitle r)
                          (T.intercalate ", " $ dartists r)
                          (dreleased r)
                          (toCoverURL r)
                          (dadded r)
                          ( fromEnum TTidal )
                          getAlbumURL
                          "Tidal"
                          Nothing -- (Just (fromString "https://listen.tidal.com/album/" <> show ( daid r )))
                          Nothing
                          0
                          0

    ds <- case getTidal p of
          TidalFile fn -> FJ.readReleases fn
          _ -> FT.readTidalReleases (getTidal p)
    let as  = toAlbum <$> ds

    putTextLn $ "Total # Tidal Albums: " <> show (length as)
    -- print $ drop (length as - 4) as

    return $ V.fromList as


instance Provider Discogs where
  readLists p = case getDiscogs p of
                  DiscogsFile _ -> FJ.readLists
                  _ -> FD.readDiscogsLists (getDiscogs p)
  readAlbums p = do
    let
        toCoverURL r = dcover r
        toFolder r = dfolder r
        getAlbumURL a = T.pack $
            "https://www.discogs.com/release/" ++ show ( albumID a )
        toAlbum r = Album (daid r)
                          (dtitle r)
                          (T.intercalate ", " $ dartists r)
                          (dreleased r)
                          (toCoverURL r)
                          (dadded r)
                          (toFolder r)
                          getAlbumURL
                          (T.intercalate ", " $ dformat r)
                          (dtidalurl r)
                          (dlocation r)
                          (drating r)
                          (dplays r)
        -- fn = "data/dall.json"

    ds <- case getDiscogs p of
          DiscogsFile fn -> FJ.readReleases fn
          _ -> FD.readDiscogsReleases (getDiscogs p)

    let as  = toAlbum <$> ds

    putTextLn $ "Total # Discogs Albums: " <> show (length as)
    -- print $ drop ( length as - 4 ) as

    return $ V.fromList as


readListAids :: Discogs -> Int -> IO ( Vector Int )
readListAids p i = case getDiscogs p of
                     DiscogsFile _ -> pure V.empty -- maybe not ok
                     _ -> FD.readListAids (getDiscogs p) i

readFolders :: Discogs -> IO ( Map Text Int )
readFolders p = case getDiscogs p of
                  DiscogsFile _ -> FJ.readFolders
                  _ -> FD.readDiscogsFolders (getDiscogs p)

-- populate the aids for folders from the folder+id in each Album
readFolderAids :: Map Text Int -> Map Int Album -> Map Text ( Int, Vector Int )
readFolderAids fm am = fam where
  -- 0: all discogs
  -- 1: uncategorized
  fam'  = M.map getFolder fm
  fam = M.insert "Tidal"   (fromEnum TTidal, allTidal)
      $ M.insert "Discogs" (fromEnum TDiscogs, allDiscogs)
      $ M.insert "All"     (fromEnum TAll, allAlbums)
        fam'
  allAlbums = sAdded
            $ V.map albumID
            $ V.fromList $ M.elems am
  allDiscogs = sAdded
              $ V.map fst
              $ V.filter (\ (_,f) -> f/=fromEnum TTidal)
              $ V.map (\a -> (albumID a, albumFolder a))
               $ V.fromList $ M.elems am
  allTidal = sAdded
           $ V.map fst
           $ V.filter (\ (_,f) -> f==fromEnum TTidal)
           $ V.map (\a -> (albumID a, albumFolder a))
           $ V.fromList $ M.elems am

  sAdded :: Vector Int -> Vector Int
  sAdded aids = V.fromList ( fst <$> sortBy ( \ (_,a) (_,b) -> comparing ( fmap albumAdded) b a ) asi ) where
    asi :: [ ( Int, Maybe Album ) ]
    asi =  map ( \aid -> ( aid, M.lookup aid am ) ) $ V.toList aids
  getFolder :: Int -> (Int, Vector Int)
  getFolder i = (i, filtFolder i)
  filtFolder :: Int -> Vector Int
  filtFolder fid = sAdded
                 $ V.map fst
                 $ V.filter (\ (_,f) -> f==fid)
                 $ V.map (\a -> (albumID a, albumFolder a))
                 $ V.fromList $ M.elems am

rereadLists :: Discogs -> IO ( Map Text (Int, Vector Int) )
rereadLists p = case getDiscogs p of
                  DiscogsFile fn -> error $ "Bug: Provider Discogs does not reread lists from files " <> toText fn
                  _ -> FD.rereadLists (getDiscogs p)
-- items[].item.type
-- "SINGLE"
-- "ALBUM"
-- "EP"

-- link : http://www.tidal.com/album/aid
--             https://listen.tidal.com/album/
--             https://www.discogs.com/release/

-- items[].item.audioQuality
-- LOSSLESS
-- HI_RES
-- HIGH

