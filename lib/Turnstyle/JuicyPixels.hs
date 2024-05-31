{-# LANGUAGE TypeFamilies #-}
module Turnstyle.JuicyPixels
    ( Image
    , loadImage
    ) where

import qualified Codec.Picture  as JP

import           Turnstyle.Spec

newtype JuicyPixels = JuicyPixels (JP.Image JP.PixelRGB8)

instance Image JuicyPixels where
    type Pixel JuicyPixels       = JP.PixelRGB8
    width      (JuicyPixels img) = JP.imageWidth img
    height     (JuicyPixels img) = JP.imageHeight img
    pixel  x y (JuicyPixels img) = JP.pixelAt img x y

loadImage :: FilePath -> IO JuicyPixels
loadImage path =
    JP.readImage path >>=
    either fail (pure . JuicyPixels . JP.convertRGB8)
