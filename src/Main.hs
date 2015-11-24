{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Main where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Metadata as Picture

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as Array
import qualified Data.Either as Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import System.FilePath.Find ((==?), (&&?))
import qualified System.FilePath.Find as FManip

main :: IO ()
main = putStrLn "hello"


listFiles :: String -> FilePath -> IO [FilePath]
listFiles ext = FManip.find (FManip.depth ==? 0) (FManip.extension ==? ext &&? FManip.fileType ==? FManip.RegularFile)


loadFiles :: [FilePath] -> IO (Array Int (Picture.DynamicImage, Picture.Metadatas))
loadFiles filepaths
    = return
    . Array.listArray (0, length filepaths - 1)
    . Either.rights
    =<< mapM Picture.readImageWithMetadata filepaths


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
    third . foldr packOne (0, Map.empty, []) . Array.assocs

    where
    packOne (index, imageSize) (page, rects, locations) =
        case Map.lookupGE imageSize rects of
            Just (key, rect : tail) ->
                let (loc, rest) = locate rect (index, imageSize)
                    m' = insertsMap rest
                       . Map.update (const $ if null tail then Nothing else Just tail) key 
                       $ rects
                in (page, m', loc : locations)

            _ ->
                let (loc, rest) = locate (newTexture (page + 1)) (index, imageSize)
                in (page + 1, insertsMap rest rects, loc : locations)

    locate rect (index, imageSize) =
        let p = page rect
            (imageWidth, imageHeight) = imageSize
            (rectWidth, rectHeight) = value rect
            (x, y) = position rect
            loc = Location p (x, y) False index
            right = if rectWidth > imageWidth
                        then Just $ Location p (x + imageWidth, y) False (rectWidth - imageWidth, imageHeight)
                        else Nothing
            bottom = if rectHeight > imageHeight
                        then Just $ Location p (x, y + imageHeight) False (rectWidth, rectHeight - imageHeight)
                        else Nothing
        in (loc, Maybe.catMaybes [right, bottom])

    newTexture p = Location p (0, 0) False textureSize

    insertsMap :: [Location (Int, Int)] -> Map (Int, Int) [Location (Int, Int)] -> Map (Int, Int) [Location (Int, Int)]
    insertsMap rects m = foldr (\rect -> Map.insertWith (++) (value rect) [rect]) m rects

    third (_, _, a) = a
