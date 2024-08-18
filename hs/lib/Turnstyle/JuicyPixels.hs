{-# LANGUAGE TypeFamilies #-}
module Turnstyle.JuicyPixels
    ( JuicyPixels (..)
    , Image
    , loadImage
    ) where

import qualified Codec.Picture   as JP

import           Turnstyle.Image (Image (..))

newtype JuicyPixels = JuicyPixels (JP.Image JP.PixelRGBA8)

instance Image JuicyPixels where
    type Pixel JuicyPixels       = JP.PixelRGBA8
    width      (JuicyPixels img) = JP.imageWidth img
    height     (JuicyPixels img) = JP.imageHeight img
    pixel  x y (JuicyPixels img) = JP.pixelAt img x y

loadImage :: FilePath -> IO JuicyPixels
loadImage path =
    JP.readImage path >>=
    either fail (pure . JuicyPixels . JP.convertRGBA8)
