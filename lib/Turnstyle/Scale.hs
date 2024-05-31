{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Turnstyle.Scale
    ( Scale (..)
    , Scaled (..)
    , autoScale
    ) where

import           Data.List.NonEmpty (NonEmpty ((:|)), group1)
import           Data.Semigroup     (sconcat)
import           Turnstyle.Image

data Scale = Scale Int Int deriving (Show)

data Scaled img = Scaled Scale img

instance Image img => Image (Scaled img) where
    type Pixel (Scaled img) = Pixel img
    width  (Scaled (Scale xs _) img) = width img  `div` xs
    height (Scaled (Scale _ ys) img) = height img `div` ys
    pixel x y (Scaled (Scale xs ys) img) = pixel (x * xs) (y * ys) img

newtype Gcd = Gcd {unGcd :: Int} deriving (Show)

instance Semigroup Gcd where
    Gcd x <> Gcd y = Gcd (gcd x y)

findScale :: (Image img, Eq (Pixel img)) => img -> Scale
findScale img = Scale (unGcd xScale) (unGcd yScale)
  where
    xScale :: Gcd
    xScale = sconcat $ fmap (Gcd . length) $ do
        y <- 0 :| [1 .. height img - 1]
        group1 $ do
            x <- 0 :| [1 .. width img - 1]
            pure $ pixel x y img

    yScale :: Gcd
    yScale = sconcat $ fmap (Gcd . length) $ do
        x <- 0 :| [1 .. width img - 1]
        group1 $ do
            y <- 0 :| [1 .. height img - 1]
            pure $ pixel x y img

autoScale :: (Image img, Eq (Pixel img)) => img -> (Scaled img)
autoScale img = Scaled (findScale img) img
