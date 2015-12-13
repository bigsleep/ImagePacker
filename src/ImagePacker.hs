{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings #-}
module ImagePacker
  ( loadFiles
  , packImages
  , writeTexture
  , toPackedImageInfos
  , PackedImageInfo
  ) where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture
import qualified Codec.Picture.RGBA8 as Picture (fromDynamicImage)

import Control.Exception (throw)
import Control.Monad (mplus)
import Control.Monad.ST (RealWorld)

import Data.Array.IArray (Array, (!))
import qualified Data.Array.IArray as Array
import qualified Data.Either as Either
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified Data.Text.Lazy.IO as LT (writeFile)
import qualified Data.Vector as V (fromList)

import ImagePacker.Types (PackedImageInfo(PackedImageInfo), Rect(..))


loadFiles :: [FilePath] -> IO (Array Int Picture.DynamicImage)
loadFiles filepaths
    = return
    . Array.listArray (0, length filepaths - 1)
    =<< mapM (either (throw . userError) return)
    =<< mapM Picture.readImage filepaths


packImages
    :: (Int, Int)
    -> Array Int (Int, Int)
    -> [Rect Int]
packImages textureSize =
    List.foldl' pack [] . sortInputs . Array.assocs

    where
    pack :: [Rect Int] -> (Int, (Int, Int)) -> [Rect Int]
    pack rects a =
        Maybe.fromMaybe (rects ++ [newRect (0, 0) textureSize a]) (tryPack a rects)

    newRect (x, y) (rw, rh) (index, (w, h)) =
        let (childL, childR) =
                if rw - w >= rh - h
                    then (Rect (x + w, y) (rw - w, rh) Nothing, Rect (x, y + h) (w, rh - h) Nothing)
                    else (Rect (x, y + h) (rw, rh - h) Nothing, Rect (x + w, y) (rw - w, h) Nothing)

        in Rect (x, y) (rw, rh) (Just (index, childL, childR))

    tryPack a [] = Nothing

    tryPack a (r : rs) =
        case tryPackOne a r of
            Just r' -> Just (r' : rs)
            Nothing -> fmap (r :) (tryPack a rs)

    tryPackOne :: (Int, (Int, Int)) -> Rect Int -> Maybe (Rect Int)

    tryPackOne a @ (index, (w, h)) r @ (Rect _ (rw, rh) Nothing) =
        if rw >= w && rh >= h
            then Just $ newRect (rectPosition r) (rectSize r) a
            else Nothing

    tryPackOne a r @ (Rect _ _ (Just (e, childL, childR))) =
        mplus
            (tryPackOne a childL >>= (\childL' -> Just r { rectElement = Just (e, childL', childR) }))
            (tryPackOne a childR >>= (\childR' -> Just r { rectElement = Just (e, childL, childR') }))

    sortInputs = List.sortBy (\(_, (lw, lh)) (_, (rw, rh)) -> compare (rw, rh) (lw, lh))
                

writeTexture :: Array Int Picture.DynamicImage -> FilePath -> (Int, Int) -> Rect Int -> IO ()
writeTexture sources destination (width, height) rect =
    do
        texture <- Picture.newMutableImage width height
        initializeTexture texture
        render texture rect
        Picture.writePng destination =<< Picture.freezeImage texture

    where
    background = Picture.PixelRGBA8 (toEnum 0) (toEnum 0) (toEnum 0) (toEnum 0)

    render :: Picture.MutableImage RealWorld Picture.PixelRGBA8 -> Rect Int -> IO ()
    render texture r @ (Rect p s (Just (e, childR, childB))) =
        do
            writePixels texture p $ sources ! e
            render texture childR
            render texture childB
    render _ _ = return ()

    writePixels texture (ox, oy) img =
        let img' = Picture.fromDynamicImage img
            w = Picture.imageWidth img'
            h = Picture.imageHeight img'
        in  mapM_
                (\(x, y, a) -> Picture.writePixel texture (ox + x) (oy + y) a)
                [(x, y, Picture.pixelAt img' x y) | x <- [0..(w - 1)], y <- [0..(h - 1)]]

    initializeTexture texture =
        mapM_
            (\(x, y) -> Picture.writePixel texture x y background)
            [(x, y) | x <- [0..(width - 1)], y <- [0..(height - 1)]]


toPackedImageInfos :: Array Int FilePath -> Array Int (Int, Int) -> [Rect Int] -> [PackedImageInfo]
toPackedImageInfos sourceNames sizes rects =
    List.concatMap f ([0..] `zip` rects)

    where
    f (t, (Rect p s (Just (i, childL, childR)))) =
        PackedImageInfo (sourceNames ! i) t p (sizes ! i) False : f (t, childL) ++ f (t, childR)

    f _ = []
