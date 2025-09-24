{-# LANGUAGE TypeFamilies #-}
module Turnstyle.Image
    ( Image (..)
    , TextImage (..)
    , textToTextImage
    ) where

import           Control.Monad       (when)
import           Data.Foldable       (for_)
import qualified Data.Text           as T
import qualified Data.Vector.Unboxed as VU

class Image img where
    type Pixel img
    width  :: img -> Int
    height :: img -> Int
    pixel  :: Int -> Int -> img -> Pixel img

data TextImage = TextImage Int Int (VU.Vector Char)

instance Image TextImage where
    type Pixel TextImage = Char
    width     (TextImage w _ _) = w
    height    (TextImage _ h _) = h
    pixel x y (TextImage w _ d) = d VU.! (y * w + x)


textToTextImage :: T.Text -> Either String TextImage
textToTextImage txt = do
    w <- parseWidth rows
    pure $ TextImage w h $ VU.fromList $ concatMap T.unpack rows
  where
    h    = length rows
    rows = T.lines txt

    parseWidth :: [T.Text] -> Either String Int
    parseWidth [] = Right 0
    parseWidth (x0 : xs) = do
        let w = T.length x0
        for_ xs $ \t -> when (T.length t /= w) $ Left "line lengths don't match"
        pure w
