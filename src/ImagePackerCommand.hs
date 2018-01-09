{-# LANGUAGE DataKinds, OverloadedStrings, TemplateHaskell #-}
module ImagePackerCommand
    ( imagePacker
    , imagePackerCommand
    , runImagePackerCommand
    ) where

import qualified Codec.Picture as Picture

import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson as DA (ToJSON(..), Value(..), eitherDecode)
import qualified Data.Aeson.Types as DA (Object)
import qualified Data.ByteString.Lazy.Char8 as LB (pack)
import qualified Data.Char
import qualified Data.Either as Either
import Data.FileEmbed (embedFile)
import qualified Data.HashMap.Strict as HM (fromList, lookup, insert, union, map)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import qualified Data.Vector as V (map, fromList)

import ImagePacker
import ImagePacker.Types

import Options.Declarative

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeFileName, dropExtension)
import System.FilePath.Find ((==?), (&&?))
import qualified System.FilePath.Find as FManip

import Text.Printf (printf)
import Text.EDE.Filters ((@:))
import qualified Text.EDE as EDE (Template, eitherParse, eitherParseFile, eitherRenderWith)


listFilePaths :: Maybe String -> FilePath -> IO [FilePath]
listFilePaths extOption = FManip.find (FManip.depth ==? 0) (f extOption)
    where
    regularFileFilter = FManip.fileType ==? FManip.RegularFile
    f (Just ext) = FManip.extension ==? ext &&? regularFileFilter
    f Nothing = regularFileFilter


renderPackedImageInfo
    :: EDE.Template
    -> DA.Object
    -> Either String LT.Text
renderPackedImageInfo template values =
    EDE.eitherRenderWith extFilters template values
    where
    extFilters = HM.fromList
        [ "toValidHtmlClassName" @: toValidHtmlClassName
        ]


outputMetadata
    :: FilePath
    -> [PackedImageInfo]
    -> MetadataSetting
    -> IO ()
outputMetadata textureOutputPath packedImageInfos (MetadataSetting mtype values) =
    do
        (template, mergedValues) <- loadSetting maybeDefinedSetting
        rendered <- handleError
            $ renderPackedImageInfo template
            $ HM.insert "items" (DA.toJSON packedImageInfos) (HM.map DA.String mergedValues)
        let outputPath = solveOutputPath mergedValues
        LT.writeFile outputPath rendered

    where
    definedTypes = map definedMetadataType definedMetadataSettings
    maybeDefinedSetting = List.lookup mtype $ zip definedTypes definedMetadataSettings
    loadSetting (Just setting) = return $ (definedMetadataTemplate setting, HM.union values $ definedMetadataValues setting)
    loadSetting _ = return . flip (,) values =<< handleError =<< EDE.eitherParseFile mtype
    solveOutputPath values' =
        Maybe.fromMaybe
            (Maybe.fromMaybe textureOutputPath (T.unpack `fmap` HM.lookup "outputDirectory" values')
                </> Maybe.fromMaybe "metadata" (T.unpack `fmap` HM.lookup "filename" values')
                <.> Maybe.fromMaybe ".txt" (T.unpack `fmap` HM.lookup "extension" values'))
            (T.unpack `fmap` HM.lookup "outputPath" values')


data DefinedTemplate = DefinedTemplate
    { definedTemplateName :: String
    , definedTemplateExtension :: String
    , definedTemplateTemplate :: EDE.Template
    }


definedMetadataSettings :: [DefinedMetadataSetting]
definedMetadataSettings =
    Either.rights
    [ toEitherDefinedMetadataSetting
        "json"
        [ "extension" .= "json"
        , "filename" .= "metadata"
        ]
        $(embedFile "templates/json.ede")

    , toEitherDefinedMetadataSetting
        "haskell"
        [ "extension" .= "hs"
        , "filename" .= "Assets"
        , "outputDirectory" .= "."
        , "moduleName" .= "Assets"
        , "typeName" .= "AssetInfo"
        ]
        $(embedFile "templates/haskell.ede")

    , toEitherDefinedMetadataSetting
        "elm"
        [ "extension" .= "elm"
        , "filename" .= "Assets"
        , "outputDirectory" .= "."
        , "moduleName" .= "Assets"
        , "typeName" .= "AssetInfo"
        ]
        $(embedFile "templates/elm.ede")

    , toEitherDefinedMetadataSetting
        "css"
        [ "extension" .= "css"
        , "filename" .= "sprite"
        ]
        $(embedFile "templates/css.ede")
    ]
    where
    toEitherDefinedMetadataSetting typeName values template =
        DefinedMetadataSetting typeName (HM.fromList values) `fmap` EDE.eitherParse template

    (.=) = (,)


toValidHtmlClassName :: T.Text -> T.Text
toValidHtmlClassName = T.pack . map toValidChar . dropExtension . T.unpack
    where
    toValidChar c | Data.Char.isAlphaNum c || c == '-' || c == '_' = c
                  | otherwise = '_'


imagePacker
    :: Maybe String
    -> (Int, Int)
    -> Int
    -> [MetadataSetting]
    -> FilePath
    -> FilePath
    -> FilePath
    -> IO ()
imagePacker
    sourceExtention
    textureSize
    spacing
    metadataSettings
    textureFileNameFormat
    inputPath
    outputPath
    = do
        inputFilePaths <- listFilePaths sourceExtention inputPath
        imgs <- loadFiles inputFilePaths
        let sizes = V.map (\x -> (Picture.imageWidth x, Picture.imageHeight x)) imgs
            rects = packImages textureSize spacing sizes
            fileNames = V.fromList . map takeFileName $ inputFilePaths
            packedImageInfos = toPackedImageInfos fileNames sizes rects

        createDirectoryIfMissing True outputPath

        mapM_ (outputMetadata outputPath packedImageInfos) metadataSettings
        mapM_ (\(i, rect) -> writeTexture imgs (renderTexturePath i) textureSize rect) ([0..] `zip` rects)

    where
    renderTexturePath :: Int -> FilePath
    renderTexturePath i = outputPath </> printf textureFileNameFormat i


imagePackerCommand
    :: Flag "" '["input-extension"] "STRING" "extension of input file name" (Maybe String)
    -> Flag "s" '["texture-size"] "INT,INT" "output texture size" (Def "1024,1024" String)
    -> Flag "m" '["metadata-settings"] "[{\"metadataType\":\"json\",\"metadataValues\":{}}]" "metadata settings" (Def "[{\"metadataType\":\"json\"}]" String)
    -> Flag "" '["texture-filename"] "STRING" "output texture filename format" (Def "texture%d.png" String)
    -> Flag "" '["spacing"] "INT" "spacing between images" (Def "1" String)
    -> Arg "INPUT_PATH" String
    -> Arg "OUTPUT_PATH" String
    -> Cmd "image packer" ()
imagePackerCommand
    sourceExtension
    textureSize
    metadataSettings
    textureFileNameFormat
    spacing
    inputPath
    outputPath
    = liftIO $ do
        metadataSettings' <- handleError . DA.eitherDecode . LB.pack $ get metadataSettings
        imagePacker
            (get sourceExtension)
            (read $ "(" ++ get textureSize ++ ")")
            (read $ get spacing)
            metadataSettings'
            (get textureFileNameFormat)
            (get inputPath)
            (get outputPath)

runImagePackerCommand :: IO ()
runImagePackerCommand = run_ imagePackerCommand

handleError :: Either String a -> IO a
handleError = either (throw . userError) return
