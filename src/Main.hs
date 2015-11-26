{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture
import qualified Codec.Picture.RGBA8 as Picture (fromDynamicImage)

import Control.Monad (mplus)
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
main = putStrLn "hello"
{-
 do
    imgs <- loadFiles =<< listFilePaths ".png" "tmp"
    let sizes = Array.amap (Picture.dynamicMap (\x -> (Picture.imageWidth x, Picture.imageHeight x))) imgs
    let textureSize = (2048, 2048)
    let locations = pack textureSize sizes
    let pages = List.groupBy (\a b -> page a == page b) locations
    print (Array.assocs sizes)
    print locations
    print pages
    writeTexture "test.png" textureSize (head pages) imgs
-}


listFilePaths :: String -> FilePath -> IO [FilePath]
listFilePaths ext = FManip.find (FManip.depth ==? 0) (FManip.extension ==? ext &&? FManip.fileType ==? FManip.RegularFile)


loadFiles :: [FilePath] -> IO (Array Int Picture.DynamicImage)
loadFiles filepaths
    = return
    . Array.listArray (0, length filepaths - 1)
    . Either.rights
    =<< mapM Picture.readImage filepaths


data Rect a = Rect
    { position :: (Int, Int)
    , size :: (Int, Int)
    , element :: Maybe (a, Rect a, Rect a)
    } deriving (Show, Eq)

packImages
    :: (Int, Int)
    -> Array Int (Int, Int)
    -> [Rect Int]
packImages textureSize =
    List.foldl' pack [] . Array.assocs

    where
    pack :: [Rect Int] -> (Int, (Int, Int)) -> [Rect Int]
    pack rects a =
        rect (0, 0) textureSize a : rects `Maybe.fromMaybe` tryPack a rects

    rect (x, y) (rw, rh) (index, (w, h)) =
        let childR = Rect (x + w, y) (rw - w, h) Nothing
            childB = Rect (x, y + h) (w, rh - h) Nothing
        in Rect (x, y) (rw, rh) (Just (index, childR, childB))

    tryPack a [] = Nothing

    tryPack a (r : rs) =
        case tryPackOne a r of
            Just r' -> Just (r' : rs)
            Nothing -> (r :) `fmap` tryPack a rs

    tryPackOne :: (Int, (Int, Int)) -> Rect Int -> Maybe (Rect Int)

    tryPackOne a @ (index, (w, h)) r @ (Rect _ (rw, rh) Nothing) =
        if rw < w || rh < h
            then Nothing
            else Just $ rect (position r) (size r) a

    tryPackOne a @ (index, (w, h)) r @ (Rect _ (rw, rh) (Just (e, childR, childB))) =
        if rw < w || rh < h
            then Nothing
            else
                tryPackOne a childR >>= \childR' -> Just r { element = Just (e, childR', childB) }
                `mplus`
                tryPackOne a childB >>= \childB' -> Just r { element = Just (e, childR, childB') }
                



{-
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

-}
