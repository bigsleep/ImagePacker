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
import qualified Data.Maybe as Maybe
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
                Nothing -> regularFileFilter
    in FManip.find (FManip.depth ==? 0) filter


renderPackedImageInfo
    :: EDE.Template
    -> String
    -> String
    -> [ImagePacker.PackedImageInfo]
    -> Either String LT.Text
renderPackedImageInfo template moduleName typeName packedImageInfos =
    EDE.eitherRender template value
    where
    value = EDE.fromPairs
        [ "moduleName" .= DA.toJSON moduleName
        , "typeName" .= DA.toJSON typeName
        , "items" .= DA.toJSON packedImageInfos
        ]


data DefinedTemplate = DefinedTemplate
    { definedTemplateName :: String
    , definedTemplateExtension :: String
    , definedTemplateTemplate :: EDE.Template
    }


templates :: [(String, DefinedTemplate)]
templates =
    Either.rights $ map convertToDefinedTemplate
    [ ("json", "json", $(embedFile "templates/json.ede"))
    , ("haskell", "hs", $(embedFile "templates/haskell.ede"))
    , ("elm", "elm", $(embedFile "templates/elm.ede"))
    ]
    where
    convertToDefinedTemplate (name, extension, bs) =
        fmap (\template -> (name, DefinedTemplate name extension template)) (EDE.eitherParse bs)


imagePacker
    :: Maybe String
    -> (Int, Int)
    -> String
    -> Maybe FilePath
    -> String
    -> String
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
    metadataModule
    metadataTypeName
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
        LT.writeFile metadataPath' =<< (handleError $ renderPackedImageInfo template metadataModule metadataTypeName packedImageInfos)
        mapM_ (\(i, rect) -> ImagePacker.writeTexture imgs (renderTexturePath i) textureSize rect) ([0..] `zip` rects)

    where
    handleError = either (throw . userError) return

    renderTexturePath :: Int -> FilePath
    renderTexturePath i = outputPath </> printf textureFileNameFormat i

    loadTemplate _ (Just path) = handleError =<< EDE.eitherParseFile path
    loadTemplate mtype _ = maybe (throw . userError $ "unknown metadata type: " ++ mtype) return $ findTemplate mtype

    findTemplate mtype = fmap definedTemplateTemplate $ List.lookup mtype templates

    findExtension mtype = Maybe.fromMaybe "" . fmap definedTemplateExtension $ List.lookup mtype templates

    metadataPath' =
        case (templatePath, metadataPath, findExtension metadataType) of
            (_, Just mpath, _) -> mpath
            (Just tpath, _, _) -> outputPath </> "metadata"
            (_, _, extension) -> outputPath </> "metadata" <.> extension


imagePackerCommand
    :: Flag "" '["input-extension"] "STRING" "extension of input file name" (Maybe String)
    -> Flag "s" '["texture-size"] "(INT,INT)" "output texture size" (Def "(1024, 1024)" String)
    -> Flag "t" '["metadata-type"] "STRING" "metadata type. one of json or elm" (Def "json" String)
    -> Flag "" '["metadata-template-file"] "STRING" "metadata template file path." (Maybe String)
    -> Flag "" '["metadata-module-name"] "STRING" "metadata module name." (Def "Assets" String)
    -> Flag "" '["metadata-type-name"] "STRING" "metadata type name." (Def "AssetInfo" String)
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
    metadataModule
    metadataTypeName
    textureFileNameFormat
    metadataPath
    inputPath
    outputPath
    = liftIO $ imagePacker 
        (get sourceExtension)
        (read $ get textureSize)
        (get metadataType)
        (get templatePath)
        (get metadataModule)
        (get metadataTypeName)
        (get textureFileNameFormat)
        (get metadataPath)
        (get inputPath)
        (get outputPath)
