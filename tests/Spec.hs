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
packImagesSpec = do
    Q.prop "packImages" $
        \imageSizes -> do
            let imageSizes' = Array.listArray (0, length imageSizes - 1) . map unImageSize $ imageSizes
                ps = ImagePacker.packImages (textureSize, textureSize) imageSizes'
                layouts = map ImagePacker.packedLayouts ps
            (List.sort . map ImagePacker.layoutImageIndex . List.concat $ layouts) `shouldBe` Array.indices imageSizes'
            mapM_ (noAnyOverlappings imageSizes') layouts

    where
    noAnyOverlappings sizes (a : b : tail) = do
        noOverlapping sizes a b
        noAnyOverlappings sizes (a : tail)
        noAnyOverlappings sizes (b : tail)

    noAnyOverlappings sizes _ = return ()

    noOverlapping :: Array.Array Int (Int, Int) -> ImagePacker.Layout -> ImagePacker.Layout -> Expectation
    noOverlapping sizes a b = do
            let (aw, ah) = sizes Array.! (ImagePacker.layoutImageIndex a)
                (bw, bh) = sizes Array.! (ImagePacker.layoutImageIndex b)
                sa = if ImagePacker.layoutRotated a then (ah, aw) else (aw, ah)
                sb = if ImagePacker.layoutRotated b then (bh, bw) else (bw, bh)
                pa = ImagePacker.layoutPosition a
                pb = ImagePacker.layoutPosition b
            when (overlapping (pa, sa) (pb, sb)) $ expectationFailure $ "overlapping found between " ++ show (pa, sa) ++ " and " ++ show (pb, sb)

    overlapping ((ax, ay), (aw, ah)) ((bx, by), (bw, bh)) =
        let (acx, acy) = (ax * 2 + aw, ay * 2 + ah)
            (bcx, bcy) = (bx * 2 + bw, by * 2 + bh)
            (dx, dy) = (abs (acx - bcx), abs (acy - bcy))
        in dx < aw + bw && dy < ah + bh

newtype ImageSize =
    ImageSize
    { unImageSize :: (Int, Int)
    } deriving (Show, Eq)

instance Q.Arbitrary ImageSize where
    arbitrary = do
        w <- Q.choose (1, textureSize)
        h <- Q.choose (1, textureSize)
        return (ImageSize (w, h))
