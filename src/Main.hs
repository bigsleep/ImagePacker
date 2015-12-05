{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Main where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture

import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson.Types as DA (ToJSON(..))
import qualified Data.Array.IArray as Array
import qualified Data.HashMap.Strict as HM (fromList)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.IO as LT (writeFile)

import qualified ImagePacker

import Options.Declarative

import System.FilePath ((</>), takeFileName)
import System.FilePath.Find ((==?), (&&?))
import qualified System.FilePath.Find as FManip

import Text.Printf (printf)

import Text.EDE ((.=))
import qualified Text.EDE as EDE (Template, eitherParseFile, eitherRender, fromPairs)


main :: IO ()
main = run_ imagePackerCommand


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



imagePacker
    :: String
    -> (Int, Int)
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> IO ()
imagePacker
    sourceExtention
    textureSize
    templatePath
    textureFileNameFormat
    metadataPath
    inputPath
    outputPath
    = do
        inputFilePaths <- listFilePaths sourceExtention inputPath
        imgs <- ImagePacker.loadFiles inputFilePaths
        let sizes = Array.amap (Picture.dynamicMap (\x -> (Picture.imageWidth x, Picture.imageHeight x))) imgs
        let rects = ImagePacker.packImages textureSize sizes
        let fileNames = Array.listArray (0, (length inputFilePaths - 1)) . map takeFileName $ inputFilePaths
        let packedImageInfos = ImagePacker.toPackedImageInfos fileNames rects
        template <- handleError =<< EDE.eitherParseFile templatePath

        LT.writeFile metadataPath =<< (handleError $ renderPackedImageInfo template packedImageInfos)
        mapM_ (\(i, rect) -> ImagePacker.writeTexture imgs (renderTexturePath i) textureSize rect) ([0..] `zip` rects)

    where
    handleError = either error return

    renderTexturePath :: Int -> FilePath
    renderTexturePath i = outputPath </> printf textureFileNameFormat i


imagePackerCommand
    :: Flag "" '["input-extension"] "STRING" "extension of input file name" (Def ".png" String)
    -> Flag "s" '["texture-size"] "(INT,INT)" "output texture size" (Def "(1024, 1024)" String)
    -> Flag "" '["metadata-template-file"] "STRING" "metadata template file path." (Def "templates/elm.ede" String)
    -> Flag "" '["texture-filename"] "STRING" "output texture filename format" (Def "texture%d.png" String)
    -> Flag "" '["metadata-path"] "STRING" "output metadata path" (Def "metadata.txt" String)
    -> Arg "INPUT PATH" String
    -> Arg "OUTPUT PATH" String
    -> Cmd "image packer" ()
imagePackerCommand
    sourceExtension
    textureSize
    templatePath
    textureFileNameFormat
    metadataPath
    inputPath
    outputPath
    = liftIO $ imagePacker 
        (get sourceExtension)
        (read $ get textureSize)
        (get templatePath)
        (get textureFileNameFormat)
        (get metadataPath)
        (get inputPath)
        (get outputPath)
