module Main where

import Control.Monad (when)

import qualified Data.Array.IArray as Array
import qualified Data.List as List

import qualified ImagePacker as ImagePacker
import qualified ImagePacker.Types as ImagePacker

import Test.Hspec (Expectation, hspec, Spec, describe, it, shouldBe, shouldSatisfy, expectationFailure)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q

main :: IO ()
main = hspec $ do
    packImagesSpec

textureSize :: Int
textureSize = 1024


packImagesSpec :: Spec
packImagesSpec =
    Q.prop "packImages" $
        \imageSizes -> do
            let imageSizes' = Array.listArray (0, length imageSizes - 1) . map unImageSize $ imageSizes
                rects = ImagePacker.packImages (textureSize, textureSize) imageSizes'
                locations = map toLocation $ rects
            (List.sort . map imageIndex . List.concat $ locations) `shouldBe` Array.indices imageSizes'
            mapM_ (noAnyOverlappings imageSizes') locations

    where
    toLocation (ImagePacker.Rect p s (Just (i, left, right))) = Location i p : toLocation left ++ toLocation right
    toLocation _ = []

    noAnyOverlappings sizes (a : b : tail) = do
        noOverlapping sizes a b
        noAnyOverlappings sizes (a : tail)
        noAnyOverlappings sizes (b : tail)

    noAnyOverlappings sizes _ = return ()

    noOverlapping :: Array.Array Int (Int, Int) -> Location -> Location -> Expectation
    noOverlapping sizes a b =
        do
            let (ax, ay) = position a
                (aw, ah) = sizes Array.! (imageIndex a)
                (acx, acy) = (ax * 2 + aw, ay * 2 + ah)

                (bx, by) = position b
                (bw, bh) = sizes Array.! (imageIndex b)
                (bcx, bcy) = (bx * 2 + bw, by * 2 + bh)

                (dx, dy) = (abs (acx - bcx), abs (acy - bcy))
                overlapping = dx < aw + bw && dy < ah + bh
            when overlapping $ expectationFailure $ "overlapping found between " ++ show ((ax, ay), (aw, ah)) ++ " and " ++ show ((bx, by), (bw, bh))

newtype ImageSize =
    ImageSize
    { unImageSize :: (Int, Int)
    } deriving (Show, Eq)

instance Q.Arbitrary ImageSize where
    arbitrary = do
        w <- Q.choose (1, textureSize)
        h <- Q.choose (1, textureSize)
        return (ImageSize (w, h))

data Location = Location
    { imageIndex :: Int
    , position :: (Int, Int)
    } deriving (Show, Eq)
