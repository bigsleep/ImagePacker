{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture

import qualified Data.Aeson.Types as DA (ToJSON(..))
import qualified Data.Array.IArray as Array
import qualified Data.HashMap.Strict as HM (fromList)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.IO as LT (writeFile)

import qualified ImagePacker

import qualified System.FilePath as F (takeFileName)
import System.FilePath.Find ((==?), (&&?))
import qualified System.FilePath.Find as FManip

import Text.EDE ((.=))
import qualified Text.EDE as EDE (Template, eitherParseFile, eitherRender, fromPairs)


main :: IO ()
main = do
    inputFilePaths <- listFilePaths ".png" "tmp"
    imgs <- ImagePacker.loadFiles inputFilePaths
    let sizes = Array.amap (Picture.dynamicMap (\x -> (Picture.imageWidth x, Picture.imageHeight x))) imgs
    let textureSize = (256, 256)
    let rects = ImagePacker.packImages textureSize sizes
    let fileNames = Array.listArray (0, (length inputFilePaths - 1)) . map F.takeFileName $ inputFilePaths
    let packedImageInfos = ImagePacker.toPackedImageInfos fileNames rects
    template <- handleError =<< EDE.eitherParseFile "templates/elm.ede"
    LT.writeFile "test.elm" =<< (handleError $ renderPackedImageInfo template packedImageInfos)
    mapM_ (\(i, rect) -> ImagePacker.writeTexture imgs ("test" ++ show i ++ ".png") textureSize rect) ([0..] `zip` rects)

    where
    handleError = either error return


listFilePaths :: String -> FilePath -> IO [FilePath]
listFilePaths ext = FManip.find (FManip.depth ==? 0) (FManip.extension ==? ext &&? FManip.fileType ==? FManip.RegularFile)


renderPackedImageInfo
    :: EDE.Template
    -> [ImagePacker.PackedImageInfo]
    -> Either String LT.Text
renderPackedImageInfo template packedImageInfos =
    EDE.eitherRender template value
    where
    value = EDE.fromPairs ["items" .= DA.toJSON packedImageInfos]
