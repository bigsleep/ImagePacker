{-# LANGUAGE DataKinds, FlexibleContexts, BangPatterns #-}
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
main =
 do
    imgs <- loadFiles =<< listFilePaths ".png" "tmp"
    let sizes = Array.amap (Picture.dynamicMap (\x -> (Picture.imageWidth x, Picture.imageHeight x))) imgs
    let textureSize = (256, 256)
    let rects = packImages textureSize sizes
    print (Array.assocs sizes)
    print (length rects)
    print rects
    mapM_ (\(i, rect) -> writeTexture imgs ("test" ++ show i ++ ".png") textureSize rect) ([0..] `zip` rects)


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
            then Just $ newRect (position r) (size r) a
            else Nothing

    tryPackOne a r @ (Rect _ _ (Just (e, childL, childR))) =
        mplus
            (tryPackOne a childL >>= (\childL' -> Just r { element = Just (e, childL', childR) }))
            (tryPackOne a childR >>= (\childR' -> Just r { element = Just (e, childL, childR') }))

    sortInputs = List.sortBy (\(_, (lw, lh)) (_, (rw, rh)) -> compare (rw, rh) (lw, lh))
                

writeTexture :: Array Int Picture.DynamicImage -> FilePath -> (Int, Int) -> Rect Int -> IO ()
writeTexture sources destination (width, height) rect =
    do
        texture <- Picture.newMutableImage width height
        initializeTexture texture
        render texture rect
        !img <- Picture.freezeImage texture
        Picture.writePng destination img
        return ()

    where
    background = Picture.PixelRGBA8 (toEnum 0) (toEnum 0) (toEnum 0) (toEnum 0)

    render :: Picture.MutableImage (PrimState IO) Picture.PixelRGBA8 -> Rect Int -> IO ()
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
            black = Picture.PixelRGBA8 (toEnum 0) (toEnum 0) (toEnum 0) (toEnum 255)
        in  mapM_
                (\(x, y, a) -> Picture.writePixel texture (ox + x) (oy + y) a)
                [(x, y, if x == 0 || x == 1 || x == w - 2 || x == w - 1 || y == 0 || y == 1 || y == h - 2 || y == h - 1 then black  else Picture.pixelAt img' x y) | x <- [0..(w - 1)], y <- [0..(h - 1)]]

    initializeTexture texture =
        mapM_
            (\(x, y) -> Picture.writePixel texture x y background)
            [(x, y) | x <- [0..(width - 1)], y <- [0..(height - 1)]]
