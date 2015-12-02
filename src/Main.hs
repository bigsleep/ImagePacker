module Main where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture

import qualified Data.Aeson.Types as DA (Value(..))
import qualified Data.Array.IArray as Array
import qualified Data.HashMap.Strict as HM (fromList)

import qualified ImagePacker

import System.FilePath.Find ((==?), (&&?))
import qualified System.FilePath.Find as FManip

import Text.EDE ((.=))
import qualified Text.EDE as EDE (eitherParseFile, eitherRender, fromPairs)


main :: IO ()
main = do
{-
    r <- EDE.eitherParseFile "templates/elm.ede"
    either error (LT.writeFile "test.elm") $ r >>= (`EDE.eitherRender` env)

    where
    env = EDE.fromPairs [ "items" .= items ]
    items = DA.Array . V.fromList $ [item, item, item, item]
    item = DA.Object . EDE.fromPairs $ ["name" .= DA.String "test", "position" .= DA.String "(0,0)", "size" .= DA.String "(1,1)", "rotated" .= DA.String "False"]
{ -}
    imgs <- ImagePacker.loadFiles =<< listFilePaths ".png" "tmp"
    let sizes = Array.amap (Picture.dynamicMap (\x -> (Picture.imageWidth x, Picture.imageHeight x))) imgs
    let textureSize = (256, 256)
    let rects = ImagePacker.packImages textureSize sizes
    print (Array.assocs sizes)
    print (length rects)
    print rects
    mapM_ (\(i, rect) -> ImagePacker.writeTexture imgs ("test" ++ show i ++ ".png") textureSize rect) ([0..] `zip` rects)
{--}

listFilePaths :: String -> FilePath -> IO [FilePath]
listFilePaths ext = FManip.find (FManip.depth ==? 0) (FManip.extension ==? ext &&? FManip.fileType ==? FManip.RegularFile)
