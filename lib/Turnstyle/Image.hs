{-# LANGUAGE TypeFamilies #-}
module Turnstyle.Image
    ( Image (..)
    ) where

class Image img where
  type Pixel img
  width  :: img -> Int
  height :: img -> Int
  pixel  :: Int -> Int -> img -> Pixel img
