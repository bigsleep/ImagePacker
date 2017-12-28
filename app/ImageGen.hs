{-# LANGUAGE DataKinds #-}
module Main where

import Codec.Picture (generateImage)
import Codec.Picture.Png (writePng)
import Codec.Picture.Types (PixelRGBA8(..))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified System.FilePath as FilePath (combine)
import Options.Declarative
import qualified System.Random as Random (newStdGen, randomIO, randomRIO)

main :: IO ()
main = run_ imageGenCommand
    
imageGenCommand
    :: Flag "o" '["output"] "STRING" "output file name prefix" (Def "testimg" String)
    -> Flag "n" '["number"] "INT" "number of output files" (Def "10" Int)
    -> Flag "d" '["destination"] "STRING" "output destination directory" (Def "." String)
    -> Flag "w" '["width-range"] "INT,INT" "range of width" (Def "32, 512" String)
    -> Flag "h" '["height-range"] "INT,INT" "range of height" (Def "32, 512" String)
    -> Cmd "Generate random size image files" ()
imageGenCommand output number destination widthR heightR =
    liftIO $ forM_ [1..(get number)] (imageGen (get output) (get destination) (read $ "(" ++ get widthR ++ ")") (read $ "(" ++ get heightR ++ ")"))

imageGen
    :: String
    -> FilePath
    -> (Int, Int)
    -> (Int, Int)
    -> Int
    -> IO ()
imageGen output destination widthR heightR index = do
    r <- Random.randomIO
    g <- Random.randomIO
    b <- Random.randomIO
    w <- Random.randomRIO widthR
    h <- Random.randomRIO heightR
    let color = PixelRGBA8 r g b (toEnum 255)
    let path = FilePath.combine destination (output ++ show index ++ ".png")
    writePng path $ generateImage (\_ _ -> color) w h
