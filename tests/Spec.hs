module Main where

import Control.Monad (when)

import qualified Data.List as List
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

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
packImagesSpec = do
    Q.prop "packImages without spacing" $ spec 0
    Q.prop "packImages with spacing" $ spec 1

    where
    spec spacing imageSizes = do
        let imageSizes' = V.fromList . map unImageSize $ imageSizes
            ps = ImagePacker.packImages (textureSize, textureSize) spacing imageSizes'
            layouts = map ImagePacker.packedLayouts ps
        (List.sort . map ImagePacker.layoutImageIndex . List.concat $ layouts) `shouldBe` [0..(V.length imageSizes' - 1)]
        mapM_ (noAnyOverlappings spacing imageSizes') layouts

    noAnyOverlappings spacing sizes (a : b : tail) = do
        noOverlapping spacing sizes a b
        noAnyOverlappings spacing sizes (a : tail)
        noAnyOverlappings spacing sizes (b : tail)

    noAnyOverlappings spacing sizes _ = return ()

    noOverlapping :: Int -> Vector (Int, Int) -> ImagePacker.Layout -> ImagePacker.Layout -> Expectation
    noOverlapping spacing sizes a b = do
            let (aw, ah) = sizes ! (ImagePacker.layoutImageIndex a)
                (bw, bh) = sizes ! (ImagePacker.layoutImageIndex b)
                sa = if ImagePacker.layoutRotated a then (ah, aw) else (aw, ah)
                sb = if ImagePacker.layoutRotated b then (bh, bw) else (bw, bh)
                pa = ImagePacker.layoutPosition a
                pb = ImagePacker.layoutPosition b
            when (overlapping spacing (pa, sa) (pb, sb)) $ expectationFailure $ "overlapping found between " ++ show (pa, sa) ++ " and " ++ show (pb, sb)

    overlapping spacing ((ax, ay), (aw, ah)) ((bx, by), (bw, bh)) =
        let (acx, acy) = (ax * 2 + aw, ay * 2 + ah)
            (bcx, bcy) = (bx * 2 + bw, by * 2 + bh)
            (dx, dy) = (abs (acx - bcx), abs (acy - bcy))
        in dx < aw + bw + spacing * 2 && dy < ah + bh + spacing * 2

newtype ImageSize =
    ImageSize
    { unImageSize :: (Int, Int)
    } deriving (Show, Eq)

instance Q.Arbitrary ImageSize where
    arbitrary = do
        w <- Q.choose (1, textureSize)
        h <- Q.choose (1, textureSize)
        return (ImageSize (w, h))
