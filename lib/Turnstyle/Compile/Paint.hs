{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Compile.Paint
    ( paint
    ) where

import qualified Codec.Picture           as JP
import           Data.List               (transpose)
import           Data.Maybe              (fromMaybe)
import           Turnstyle.Compile.Shape
import           Turnstyle.TwoD

paint :: (Pos -> Maybe Int) -> Shape -> JP.Image JP.PixelRGB8
paint colors s = JP.generateImage
    (\x0 y0 -> fromMaybe background $
        let x1 = x0 - offsetX
            y1 = y0 - offsetY in
        if x1 >= 0 && x1 < sWidth s && y1 >= 0 && y1 < sHeight s
            then (palette !!) <$> colors (Pos x1 y1)
            else Nothing)
    (sWidth s)
    (spacingHeight * 2 + 1)
  where
    topHeight     = sEntrance s
    bottomHeight  = sHeight s - sEntrance s - 1
    spacingHeight = max topHeight bottomHeight
    offsetX       = 0
    offsetY       = spacingHeight - sEntrance s
    background    = JP.PixelRGB8 255 255 255

palette :: [JP.PixelRGB8]
palette = concat $ transpose
    [ [JP.PixelRGB8 c 0 0 | c <- steps]
    , [JP.PixelRGB8 0 c 0 | c <- steps]
    , [JP.PixelRGB8 0 0 c | c <- steps]
    , [JP.PixelRGB8 c c 0 | c <- steps]
    , [JP.PixelRGB8 0 c c | c <- steps]
    , [JP.PixelRGB8 c 0 c | c <- steps]
    ]
  where
    steps = reverse $ [63, 127 .. 255]
