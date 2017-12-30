{-# LANGUAGE OverloadedStrings #-}
module ImagePacker.Types
  ( PackedImageInfo(..)
  , MetadataSetting(..)
  , DefinedMetadataSetting(..)
  , Packed(..)
  , Layout(..)
  , Rect(..)
  ) where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.:), (.:?), (.!=), (.=))
import qualified Data.HashMap.Strict as HM (HashMap, empty)
import Data.Text (Text)
import qualified Text.EDE as EDE (Template)

data PackedImageInfo = PackedImageInfo
    { piiSourceName :: !String
    , piiTextureIndex :: !Int
    , piiPosition :: !(Int, Int)
    , piiSize :: !(Int, Int)
    , piiRotated :: !Bool
    } deriving (Show, Eq)

instance FromJSON PackedImageInfo where
    parseJSON (Object a) =
        PackedImageInfo
        <$> a .: "sourceName"
        <*> a .: "textureIndex"
        <*> a .: "position"
        <*> a .: "size"
        <*> a .: "rotated"

    parseJSON _ = mzero

instance ToJSON PackedImageInfo where
    toJSON a =
        object
            [ "sourceName" .= piiSourceName a
            , "textureIndex" .= piiTextureIndex a
            , "position" .= piiPosition a
            , "size" .= piiSize a
            , "rotated" .= piiRotated a
            ]


data MetadataSetting =
    MetadataSetting
    { metadataType :: !String
    , metadataValues :: !(HM.HashMap Text Text)
    } deriving (Show, Eq)


instance FromJSON MetadataSetting where
    parseJSON (Object a) =
        MetadataSetting
        <$> a .: "metadataType"
        <*> a .:? "metadataValues" .!= HM.empty

    parseJSON _ = mzero

instance ToJSON MetadataSetting where
    toJSON a =
        object
            [ "metadataType" .= metadataType a
            , "metadataValues" .= metadataValues a
            ]


data DefinedMetadataSetting =
    DefinedMetadataSetting
    { definedMetadataType :: String
    , definedMetadataValues :: HM.HashMap Text Text
    , definedMetadataTemplate :: EDE.Template
    }


data Packed = Packed
    { packedLayouts :: ![Layout]
    , packedSpaces :: ![Rect]
    } deriving (Show, Eq)


data Layout = Layout
    { layoutImageIndex :: !Int
    , layoutPosition :: !(Int, Int)
    , layoutRotated :: !Bool
    } deriving (Show, Eq)

data Rect = Rect
    { rectArea :: !Int
    , rectWidth :: !Int
    , rectHeight :: !Int
    , rectPosition :: !(Int, Int)
    } deriving (Show, Eq, Ord)
