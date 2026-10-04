{-# LANGUAGE TypeFamilies #-}
module Turnstyle.Image
    ( Image (..)
    , AsciiImage (..)
    , textToAsciiImage
    , asciiImageToText
    , imageToAsciiImage
    ) where

import           Control.Monad       (when)
import           Control.Monad.State (state, State, evalState)
import           Data.Foldable       (for_)
import qualified Data.Map as M
import qualified Data.Text           as T
import qualified Data.Vector.Unboxed as VU
import qualified Codec.Picture   as JP

class Image img where
    type Pixel img
    width  :: img -> Int
    height :: img -> Int
    pixel  :: Int -> Int -> img -> Pixel img

data AsciiImage = AsciiImage Int Int (VU.Vector Char)

instance Image AsciiImage where
    type Pixel AsciiImage = Char
    width     (AsciiImage w _ _) = w
    height    (AsciiImage _ h _) = h
    pixel x y (AsciiImage w _ d) = d VU.! (y * w + x)

asciiImageToText :: AsciiImage -> T.Text
asciiImageToText img = T.unlines $ do
    y <- [0 .. height img - 1]
    pure $ T.pack [pixel x y img |  x <- [0 .. width img - 1]]

textToAsciiImage :: T.Text -> Either String AsciiImage
textToAsciiImage txt = do
    w <- parseWidth rows
    pure $ AsciiImage w h $ VU.fromList $ concatMap T.unpack rows
  where
    h    = length rows
    rows = T.lines txt

    parseWidth :: [T.Text] -> Either String Int
    parseWidth [] = Right 0
    parseWidth (x0 : xs) = do
        let w = T.length x0
        for_ xs $ \t -> when (T.length t /= w) $ Left "line lengths don't match"
        pure w

imageToAsciiImage :: [Char] -> JP.Image JP.PixelRGBA8 -> AsciiImage
imageToAsciiImage palette img = AsciiImage w h $
    evalState chars (M.empty, palette)
    -- go M.empty palette 0 0
  where
    w = JP.imageWidth img
    h = JP.imageHeight img

    chars :: State (M.Map JP.PixelRGBA8 Char, [Char]) (VU.Vector Char)
    chars = VU.generateM (w * h) $ \idx -> do
        let (y, x) = idx `divMod` w
            p      = JP.pixelAt img x y
        state $ \(known, fresh) -> case M.lookup p known of
            Just c -> (c, (known, fresh))
            Nothing -> case fresh of
                []       -> error "ran out of fresh characters"
                (f : fs) -> (f, (M.insert p f known, fs))
    {-
    go :: M.Map JP.PixelRGBA8 Char -> [Char] -> Int -> Int -> String
    go chars fresh x y
        | x >= JP.imageWidth img  = '\n' : go chars fresh 0 (y + 1)
        | y >= JP.imageHeight img = []
        | otherwise               =
            let p = JP.pixelAt img x y in
            case M.lookup p chars of
                Just c -> c : go chars fresh (x + 1) y
                Nothing -> case fresh of
                    (f : fs) -> f : go (M.insert p f chars) fs (x + 1) y
                    []       -> error "ran out of fresh characters"
    -}
