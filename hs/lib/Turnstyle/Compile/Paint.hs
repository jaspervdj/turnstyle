{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Compile.Paint
    ( paint
    , defaultPalette
    ) where

import qualified Codec.Picture           as JP
import           Data.List               (transpose)
import           Data.Maybe              (fromMaybe)
import           Turnstyle.Compile.Shape
import           Turnstyle.TwoD

defaultPalette :: [JP.PixelRGBA8]
defaultPalette = concat $ transpose
    [ [JP.PixelRGBA8 c 0 0 255 | c <- steps]
    , [JP.PixelRGBA8 0 c 0 255 | c <- steps]
    , [JP.PixelRGBA8 0 0 c 255 | c <- steps]
    , [JP.PixelRGBA8 c c 0 255 | c <- steps]
    , [JP.PixelRGBA8 0 c c 255 | c <- steps]
    , [JP.PixelRGBA8 c 0 c 255 | c <- steps]
    , [JP.PixelRGBA8 c r 0 255 | (c, r) <- zip steps (reverse steps)]
    , [JP.PixelRGBA8 0 c r 255 | (c, r) <- zip steps (reverse steps)]
    , [JP.PixelRGBA8 c 0 r 255 | (c, r) <- zip steps (reverse steps)]
    ]
  where
    steps = reverse [15, 31 .. 255]

paint
    :: (Pos -> Maybe JP.PixelRGBA8) -> Shape ann
    -> JP.Image JP.PixelRGBA8
paint colors s = JP.generateImage
    (\x y -> fromMaybe background $
        if x >= 0 && x < sWidth s && y >= 0 && y < sHeight s
            then colors (Pos x y)
            else Nothing)
    (sWidth s)
    (sHeight s)
  where
    background = JP.PixelRGBA8 0 0 0 0
