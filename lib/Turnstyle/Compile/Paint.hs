{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Compile.Paint
    ( paint
    ) where

import qualified Codec.Picture           as JP
import           Data.List               (foldl', transpose)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import qualified Data.Set                as S
import           Turnstyle.Compile.Shape
import           Turnstyle.TwoD

paint :: Shape -> JP.Image JP.PixelRGB8
paint s = JP.generateImage
    (\x0 y0 -> fromMaybe background $
        let x1 = x0 - offsetX
            y1 = y0 - offsetY in
        if x1 >= 0 && x1 < sWidth s && y1 >= 0 && y1 < sHeight s
            then M.lookup (Pos x1 y1) colors
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
    colors        = fmap (palette !!) $ solve $ sConstraints s

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

solve :: forall p. Ord p => [ColorConstraint p] -> M.Map p Int
solve constraints = remap $
    foldl' (\acc (p, q) -> fromMaybe acc $ unify p q acc) roots $
    [(p, q) | p <- S.toList allPos, q <- S.toList allPos]
  where
    allPos :: S.Set p
    allPos = S.fromList $ do
        constraint <- constraints
        case constraint of
            Eq    p q -> [p, q]
            NotEq p q -> [p, q]

    roots :: M.Map p p
    roots =
        foldl' go (M.fromList [(p, p) | p <- S.toList allPos])  constraints
      where
        go acc (NotEq _ _) = acc
        go acc (Eq p q)    =
            fmap (\c -> if c == pc || c == qc then pc else c) acc
          where
            pc = acc M.! p
            qc = acc M.! q

    unify :: p -> p -> M.Map p p -> Maybe (M.Map p p)
    unify p q acc
        | pr == qr           = Just acc
        | all ok constraints =
            Just $ fmap (\r -> if r == pr || r == qr then pr else r) acc
        | otherwise          = Nothing
      where
        pr = acc M.! p
        qr = acc M.! q

        ok (Eq _ _) = True
        ok (NotEq a b)
            | pr == ar && qr == br = False
            | pr == br && qr == ar = False
            | otherwise            = True
          where
            ar = acc M.! a
            br = acc M.! b

    remap :: M.Map p p -> M.Map p Int
    remap m0 = go M.empty 0 $ M.toList m0
      where
        go acc _     []             = (acc M.!) <$> m0
        go acc fresh ((_, c) : pcs) = case M.lookup c acc of
            Just _  -> go acc fresh pcs
            Nothing -> go (M.insert c fresh acc) (fresh + 1) pcs
