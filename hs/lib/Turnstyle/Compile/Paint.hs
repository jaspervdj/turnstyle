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
    :: (Pos -> Maybe JP.PixelRGBA8) -> Shape
    -> JP.Image JP.PixelRGBA8
paint colors s = JP.generateImage
    (\x0 y0 -> fromMaybe background $
        let x1 = x0 - offsetX
            y1 = y0 - offsetY in
        if x1 >= 0 && x1 < sWidth s && y1 >= 0 && y1 < sHeight s
            then colors (Pos x1 y1)
            else Nothing)
    (sWidth s)
    (spacingHeight * 2 + 1)
  where
    topHeight     = sEntrance s
    bottomHeight  = sHeight s - sEntrance s - 1
    spacingHeight = max topHeight bottomHeight
    offsetX       = 0
    offsetY       = spacingHeight - sEntrance s
    background    = JP.PixelRGBA8 0 0 0 0
