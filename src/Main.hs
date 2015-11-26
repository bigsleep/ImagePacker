{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture
import qualified Codec.Picture.RGBA8 as Picture (fromDynamicImage)

import Control.Monad.Primitive (PrimState)

import Data.Array.IArray (Array, (!))
import qualified Data.Array.IArray as Array
import qualified Data.Either as Either
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import Debug.Trace (trace)

import System.FilePath.Find ((==?), (&&?))
import qualified System.FilePath.Find as FManip

main :: IO ()
main = do
    imgs <- loadFiles =<< listFilePaths ".png" "tmp"
    let sizes = Array.amap (Picture.dynamicMap (\x -> (Picture.imageWidth x, Picture.imageHeight x))) imgs
    let textureSize = (2048, 2048)
    let locations = pack textureSize sizes
    let pages = List.groupBy (\a b -> page a == page b) locations
    print (Array.assocs sizes)
    print locations
    print pages
    writeTexture "test.png" textureSize (head pages) imgs


listFilePaths :: String -> FilePath -> IO [FilePath]
listFilePaths ext = FManip.find (FManip.depth ==? 0) (FManip.extension ==? ext &&? FManip.fileType ==? FManip.RegularFile)


loadFiles :: [FilePath] -> IO (Array Int Picture.DynamicImage)
loadFiles filepaths
    = return
    . Array.listArray (0, length filepaths - 1)
    . Either.rights
    =<< mapM Picture.readImage filepaths


data Location a = Location
    { page :: Int
    , position :: (Int, Int)
    , rotated :: Bool
    , value :: a
    } deriving (Show, Eq)

pack
    :: (Int, Int)
    -> Array Int (Int, Int)
    -> [Location Int]
pack textureSize =
    third . List.foldl' packOne (0, Map.empty, []) . take 10 . Array.assocs

    where
    packOne (page, rects, locations) (index, imageSize) =
        case Map.lookupGE imageSize rects of
            Just (key, rect : tail) ->
                let (loc, rest) = locate rect (index, imageSize)
                    m' = insertsMap rest
                       . Map.update (const $ if null tail then Nothing else Just tail) key 
                       $ rects
                    m'' = trace ("lookupGE: " ++ show imageSize ++ " <= " ++ show key) (insertsMap rest m')
                in (page, m'', loc : locations)

            _ ->
                let (loc, rest) = locate (newTexture (page + 1)) (index, imageSize)
                in (page + 1, insertsMap rest rects, loc : locations)

    locate rect (index, imageSize) =
        let p = page rect
            (imageWidth, imageHeight) = imageSize
            (rectWidth, rectHeight) = value rect
            (x, y) = position rect
            loc = Location p (x, y) False index
            right = if rectWidth > imageWidth && rectHeight > imageHeight
                        then Just $ Location p (x + imageWidth, y) False (rectWidth - imageWidth, imageHeight)
                        else Nothing
            bottom = if rectHeight > imageHeight
                        then Just $ Location p (x, y + imageHeight) False (rectWidth, rectHeight - imageHeight)
                        else Nothing
        in (loc, Maybe.catMaybes [right, bottom])

    newTexture p = Location p (0, 0) False textureSize

    insertsMap :: [Location (Int, Int)] -> Map (Int, Int) [Location (Int, Int)] -> Map (Int, Int) [Location (Int, Int)]
    insertsMap rects m = List.foldl' (\n rect -> Map.insertWith (++) (value rect) [rect] n) m rects

    third (_, _, a) = a


writeTexture :: FilePath -> (Int, Int) -> [Location Int] -> Array Int Picture.DynamicImage -> IO ()
writeTexture destination (width, height) locations sources =
    do
        texture <- Picture.newMutableImage width height
        mapM_ (render texture) locations
        Picture.writePng destination =<< Picture.freezeImage texture

    where
    render :: Picture.MutableImage (PrimState IO) Picture.PixelRGBA8 -> Location Int -> IO ()
    render texture location =
        writePixels texture (position location) $
        sources ! value location

    writePixels texture (ox, oy) img =
        let img' = Picture.fromDynamicImage img
            w = Picture.imageWidth img'
            h = Picture.imageHeight img'
        in  mapM_
                (\(x, y, a) -> Picture.writePixel texture (ox + x) (oy + y) a)
                [(x, y, Picture.pixelAt img' x y) | x <- [0..(w - 1)], y <- [0..(h - 1)]]

