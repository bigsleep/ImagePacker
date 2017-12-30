{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings #-}
module ImagePacker
  ( PackedImageInfo
  , hasIntersection
  , loadFiles
  , packImages
  , toPackedImageInfos
  , writeTexture
  ) where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture

import Control.Exception (throw)
import Control.Monad (mplus)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (RealWorld)

import qualified Data.Either as Either
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified Data.Text.Lazy.IO as LT (writeFile)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V (fromList, toList, zip)
import qualified Data.Vector.Generic as V (imapM_, slice, copy)
import qualified Data.Vector.Storable as SV (fromList, unsafeCast)
import qualified Data.Vector.Storable.Mutable as MV (slice, unsafeCast, write)
import Data.Word (Word32)

import ImagePacker.Types

import Debug.Trace (trace)


loadFiles :: [FilePath] -> IO (Vector(Picture.Image Picture.PixelRGBA8))
loadFiles = fmap V.fromList . mapM loadFile
    where
    loadFile filepath = do
        a <- Picture.readImage filepath
        image <- either (throw . userError) return a
        return (Picture.convertRGBA8 image)

packImages
    :: (Int, Int)
    -> Vector (Int, Int)
    -> [Packed]
packImages textureSize xs =
    List.foldl' pack [] . sortInputs . zip [0..] . V.toList $ xs

    where
    pack :: [Packed] -> (Int, (Int, Int)) -> [Packed]
    pack ps a =
        Maybe.fromMaybe (ps ++ [newRegion a]) (tryPack a ps)

    newRegion (index, (w, h)) =
        let layouts = Layout index (0, 0) False : []
            spaces = relocateSpaces (0, 0) (w, h) (newRect (0, 0) textureSize)
        in Packed layouts spaces

    tryPack a [] = Nothing

    tryPack a (r : rs) =
        case tryPackOne a r of
            Just r' -> Just (r' : rs)
            Nothing -> fmap (r :) (tryPack a rs)

    sortInputs = List.sortBy (\(_, (lw, lh)) (_, (rw, rh)) -> compare (rw * rh) (lw * lh))

tryPackOne :: (Int, (Int, Int)) -> Packed -> Maybe Packed
tryPackOne a @ (index, (w, h)) (Packed layouts spaces) = do
    (Rect _ rw rh rp, rotated) <- s1 `mplus` s2
    let layout = Layout index rp rotated
        size = if rotated then (h, w) else (w, h)
        (intersectSpaces, restSpaces) = List.partition (hasIntersection rp size) spaces
        relocated = removeInclusion . concatMap (relocateSpaces rp size) $ intersectSpaces
    return $ Packed (layout : layouts) (restSpaces ++ relocated)
    where
    s1 = List.find locatable $ zip spaces (repeat False)
    s2 = List.find locatable $ zip spaces (repeat True)
    locatable ((Rect _ rw rh _), False) = rw >= w && rh >= h
    locatable ((Rect _ rw rh _), True) = rh >= w && rw >= h

hasIntersection :: (Int, Int) -> (Int, Int) -> Rect -> Bool
hasIntersection (x, y) (w, h) (Rect _ rw rh (rx, ry)) = dx < w + rw && dy < h + rh
    where
    (cx, cy) = (x * 2 + w, y * 2 + h)
    (rcx, rcy) = (rx * 2 + rw, ry * 2 + rh)
    (dx, dy) = (abs (cx - rcx), abs (cy - rcy))

relocateSpaces :: (Int, Int) -> (Int, Int) -> Rect -> [Rect]
relocateSpaces p s r =
    horizontalSpaces p s r ++ verticalSpaces p s r

horizontalSpaces :: (Int, Int) -> (Int, Int) -> Rect -> [Rect]
horizontalSpaces (x, y) (w, h) (Rect a rw rh (rx, ry))
    | ry < y && (y + h) < (ry + rh) = [s1, s2]
    | ry < y = [s1]
    | (y + h) < (ry + rh) = [s2]
    | otherwise = []
    where
    s1 = newRect (rx, ry) (rw, y - ry)
    s2 = newRect (rx, y + h) (rw, ry + rh - y - h)

verticalSpaces :: (Int, Int) -> (Int, Int) -> Rect -> [Rect]
verticalSpaces (x, y) (w, h) (Rect a rw rh (rx, ry))
    | rx < x && (x + w) < (rx + rw) = [s1, s2]
    | rx < x = [s1]
    | (x + w) < (rx + rw) = [s2]
    | otherwise = []
    where
    s1 = newRect (rx, ry) (x - rx, rh)
    s2 = newRect (x + w, ry) (rx + rw - x - w, rh)

removeInclusion :: [Rect] -> [Rect]
removeInclusion = removeInclusion' . List.sort
    where
    removeInclusion' [] = []
    removeInclusion' (x : xs)
        | any (inclusion x) xs = removeInclusion' xs
        | otherwise = x : removeInclusion' xs
    inclusion (Rect _ w h (x, y)) (Rect _ rw rh (rx, ry)) =
        rx <= x && x + w <= rx + rw && ry <= y && y + h <= ry + rh

newRect :: (Int, Int) -> (Int, Int) -> Rect
newRect p (w, h) = Rect (w * h) w h p

writeTexture :: Vector (Picture.Image Picture.PixelRGBA8) -> FilePath -> (Int, Int) -> Packed -> IO ()
writeTexture sources destination (width, height) (Packed layouts _) = do
    texture <- Picture.newMutableImage width height
    mapM_ (render texture) layouts
    Picture.writePng destination =<< Picture.freezeImage texture

    where
    render texture (Layout index p True) =
        writePixels texture p True $ sources ! index

    render texture (Layout index p False) =
        writeSubImage texture p $ sources ! index

    writePixels texture (ox, oy) rotated img =
        let imageData = SV.unsafeCast $ Picture.imageData img
            textureData = MV.unsafeCast $ Picture.mutableImageData texture
            w = Picture.imageWidth img
            h = Picture.imageHeight img
            componentCount = Picture.componentCount (undefined :: Picture.PixelRGBA8)
            write True i a =
                let (y, x) = divMod i w
                    (x', y') = (ox + h - y - 1, oy + x)
                in writeAt (x' + y' * width) a
            write False i a =
                let (y, x) = divMod i w
                    (x', y') = (ox + x, oy + y)
                in writeAt (x' + y' * width) a
            writeAt i a = MV.write textureData i (a :: Word32)
        in V.imapM_ (write rotated) imageData

writeSubImage
    :: (PrimMonad m)
    => Picture.MutableImage (PrimState m) Picture.PixelRGBA8 -> (Int, Int) -> Picture.Image Picture.PixelRGBA8 -> m ()
writeSubImage target (x, y) source =
    mapM_ (uncurry V.copy) (zip targetSlices sourceSlices)
    where
    componentCount = Picture.componentCount (undefined :: Picture.PixelRGBA8)
    Picture.MutableImage targetWidth targetHeight targetData = target
    targetSliceStarts = [(x + (y + dy) * targetWidth) * componentCount | dy <- [0..(sourceHeight - 1)]]
    lineSize = sourceWidth * componentCount
    targetSlices = map (\s -> MV.slice s lineSize targetData) targetSliceStarts

    Picture.Image sourceWidth sourceHeight sourceData = source
    sourceSliceStarts = [dy * sourceWidth * componentCount | dy <- [0..(sourceHeight - 1)]]
    sourceSlices = map (\s -> V.slice s lineSize sourceData) sourceSliceStarts


toPackedImageInfos :: Vector FilePath -> Vector (Int, Int) -> [Packed] -> [PackedImageInfo]
toPackedImageInfos sourceNames sizes ps =
    List.concatMap f ([0..] `zip` ps)

    where
    f (t, Packed layouts _) = map (g t) layouts

    g t (Layout index p _) =
        PackedImageInfo (sourceNames ! index) t p (sizes ! index) False
