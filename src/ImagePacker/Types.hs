{-# LANGUAGE OverloadedStrings #-}
module ImagePacker.Types
  ( PackedImageInfo(..)
  ) where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.:), (.=))

data PackedImageInfo = PackedImageInfo
    { sourceName :: String
    , textureName :: String
    , position :: (Int, Int)
    , size :: (Int, Int)
    , rotated :: Bool
    } deriving (Show, Eq)

instance FromJSON PackedImageInfo where
    parseJSON (Object a) =
        PackedImageInfo
        <$> a .: "sourceName"
        <*> a .: "textureName"
        <*> a .: "position"
        <*> a .: "size"
        <*> a .: "rotated"

    parseJSON _ = mzero

instance ToJSON PackedImageInfo where
    toJSON a =
        object
            [ "sourceName" .= sourceName a
            , "textureName" .= textureName a
            , "position" .= position a
            , "size" .= size a
            , "rotated" .= rotated a
            ]
