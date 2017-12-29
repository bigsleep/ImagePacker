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

import ImagePacker.Types

import Debug.Trace (trace)


loadFiles :: [FilePath] -> IO (Array Int Picture.DynamicImage)
loadFiles filepaths
    = return
    . Array.listArray (0, length filepaths - 1)
    =<< mapM (either (throw . userError) return)
    =<< mapM Picture.readImage filepaths


packImages
    :: (Int, Int)
    -> Array Int (Int, Int)
    -> [Packed]
packImages textureSize =
    List.foldl' pack [] . sortInputs . Array.assocs

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

    sortInputs = List.sortBy (\(_, (lw, lh)) (_, (rw, rh)) -> compare (rw, rh) (lw, lh))

tryPackOne :: (Int, (Int, Int)) -> Packed -> Maybe Packed
tryPackOne a @ (index, (w, h)) (Packed layouts spaces)
    | null locatables = Nothing
    | otherwise = Just packed
    where
    locatables = filter locatable $ zip (repeat (w, h)) spaces ++ zip (repeat (h, w)) spaces
    locatable ((w', h'), (Rect _ rw rh _)) = rw >= w' && rh >= h'
    locateImpact (s, (Rect _ _ _ rp)) = List.sortBy (flip compare) . filter (hasIntersection rp s) $ spaces
    compare' (_, a) (_, b) = compare a b
    minImpact = fst . List.minimumBy compare' $ zip locatables (map locateImpact locatables)
    pos = rectPosition . snd $ minImpact
    size = fst minImpact
    rotated = fst minImpact /= (w, h)
    layout = Layout index pos rotated
    (intersectSpaces, restSpaces) = List.partition (hasIntersection pos size) spaces
    relocated = removeInclusion . concatMap (relocateSpaces pos size) $ intersectSpaces
    packed = Packed (layout : layouts) (restSpaces ++ relocated)

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

writeTexture :: Array Int Picture.DynamicImage -> FilePath -> (Int, Int) -> Packed -> IO ()
writeTexture sources destination (width, height) (Packed layouts _) = do
    texture <- Picture.newMutableImage width height
    initializeTexture texture
    mapM_ (render texture) layouts
    Picture.writePng destination =<< Picture.freezeImage texture

    where
    background = Picture.PixelRGBA8 (toEnum 0) (toEnum 0) (toEnum 0) (toEnum 0)

    render texture (Layout index p rotated) = do
        writePixels texture p rotated $ sources ! index

    writePixels texture (ox, oy) rotated img =
        let img' = Picture.convertRGBA8 img
            w = Picture.imageWidth img'
            h = Picture.imageHeight img'
            src = [(x, y, Picture.pixelAt img' x y) | x <- [0..(w - 1)], y <- [0..(h - 1)]]
            write (x, y, a) = if rotated
                then Picture.writePixel texture (ox + (h - y - 1)) (oy + x) a
                else Picture.writePixel texture (ox + x) (oy + y) a
        in  mapM_ write src

    initializeTexture texture =
        mapM_
            (\(x, y) -> Picture.writePixel texture x y background)
            [(x, y) | x <- [0..(width - 1)], y <- [0..(height - 1)]]


toPackedImageInfos :: Array Int FilePath -> Array Int (Int, Int) -> [Packed] -> [PackedImageInfo]
toPackedImageInfos sourceNames sizes ps =
    List.concatMap f ([0..] `zip` ps)

    where
    f (t, Packed layouts _) = map (g t) layouts

    g t (Layout index p _) =
        PackedImageInfo (sourceNames ! index) t p (sizes ! index) False
