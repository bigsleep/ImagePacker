{-# LANGUAGE DataKinds, OverloadedStrings, TemplateHaskell #-}
module Main where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture

import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson.Types as DA (ToJSON(..))
import qualified Data.Array.IArray as Array
import qualified Data.Either as Either
import Data.FileEmbed (embedFile)
import qualified Data.HashMap.Strict as HM (fromList)
import qualified Data.List as List
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.IO as LT (writeFile)

import qualified ImagePacker

import Options.Declarative

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeFileName)
import System.FilePath.Find ((==?), (&&?))
import qualified System.FilePath.Find as FManip

import Text.Printf (printf)
import Text.EDE ((.=))
import qualified Text.EDE as EDE (Template, eitherParse, eitherParseFile, eitherRender, fromPairs)


main :: IO ()
main = run_ imagePackerCommand


listFilePaths :: Maybe String -> FilePath -> IO [FilePath]
listFilePaths extOption =
    let regularFileFilter = FManip.fileType ==? FManip.RegularFile
        filter =
            case extOption of
                Just ext -> FManip.extension ==? ext &&? regularFileFilter
                Nothing -> FManip.fileType ==? FManip.RegularFile
    in FManip.find (FManip.depth ==? 0) filter


renderPackedImageInfo
    :: EDE.Template
    -> [ImagePacker.PackedImageInfo]
    -> Either String LT.Text
renderPackedImageInfo template packedImageInfos =
    EDE.eitherRender template value
    where
    value = EDE.fromPairs ["items" .= DA.toJSON packedImageInfos]


templates :: [(String, EDE.Template)]
templates = Either.rights $ map (\(name, bs) -> fmap ((,) name)  (EDE.eitherParse bs))
    [ ("json", $(embedFile "templates/json.ede"))
    , ("elm", $(embedFile "templates/elm.ede"))
    ]


imagePacker
    :: Maybe String
    -> (Int, Int)
    -> String
    -> Maybe FilePath
    -> FilePath
    -> Maybe FilePath
    -> FilePath
    -> FilePath
    -> IO ()
imagePacker
    sourceExtention
    textureSize
    metadataType
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
        let packedImageInfos = ImagePacker.toPackedImageInfos fileNames sizes rects
        template <- loadTemplate metadataType templatePath

        createDirectoryIfMissing True outputPath
        LT.writeFile metadataPath' =<< (handleError $ renderPackedImageInfo template packedImageInfos)
        mapM_ (\(i, rect) -> ImagePacker.writeTexture imgs (renderTexturePath i) textureSize rect) ([0..] `zip` rects)

    where
    handleError = either (throw . userError) return

    renderTexturePath :: Int -> FilePath
    renderTexturePath i = outputPath </> printf textureFileNameFormat i

    loadTemplate _ (Just path) = handleError =<< EDE.eitherParseFile path
    loadTemplate mtype _ = maybe (throw . userError $ "unknown metadata type: " ++ mtype) return $ List.lookup mtype templates

    metadataPath' =
        case (templatePath, metadataPath) of
            (_, Just mpath) -> mpath
            (Just tpath, _) -> outputPath </> "metadata"
            _ -> outputPath </> ("metadata" <.> metadataType)


imagePackerCommand
    :: Flag "" '["input-extension"] "STRING" "extension of input file name" (Maybe String)
    -> Flag "s" '["texture-size"] "(INT,INT)" "output texture size" (Def "(1024, 1024)" String)
    -> Flag "t" '["metadata-type"] "STRING" "metadata type. one of json or elm" (Def "json" String)
    -> Flag "" '["metadata-template-file"] "STRING" "metadata template file path." (Maybe String)
    -> Flag "" '["texture-filename"] "STRING" "output texture filename format" (Def "texture%d.png" String)
    -> Flag "" '["metadata-path"] "STRING" "output metadata path" (Maybe String)
    -> Arg "INPUT_PATH" String
    -> Arg "OUTPUT_PATH" String
    -> Cmd "image packer" ()
imagePackerCommand
    sourceExtension
    textureSize
    metadataType
    templatePath
    textureFileNameFormat
    metadataPath
    inputPath
    outputPath
    = liftIO $ imagePacker 
        (get sourceExtension)
        (read $ get textureSize)
        (get metadataType)
        (get templatePath)
        (get textureFileNameFormat)
        (get metadataPath)
        (get inputPath)
        (get outputPath)
