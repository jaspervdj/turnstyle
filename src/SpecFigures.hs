{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies              #-}
import           Diagrams.Backend.SVG
import           Diagrams.Prelude

data Pixel = A | B | C | D deriving (Eq, Show)

colorToStyle :: Pixel -> Colour Double
colorToStyle A = mistyrose
colorToStyle B = lightsteelblue
colorToStyle C = greenyellow
colorToStyle D = coral

data Arrow
    = NoArrow
    | Arrow
    | DashedArrow
    deriving (Eq, Show)

data Turnstyle = Turnstyle
    { leftPixel       :: Pixel
    , leftArrow       :: Arrow
    , leftLabel       :: Maybe String
    , leftCircle      :: Bool
    , centerPixel     :: Pixel
    , centerArrow     :: Arrow
    , centerLabel     :: Maybe String
    , centerCircle    :: Bool
    , frontPixel      :: Pixel
    , frontArrow      :: Arrow
    , frontLabel      :: Maybe String
    , frontCircle     :: Bool
    , rightPixel      :: Pixel
    , rightArrow      :: Arrow
    , rightLabel      :: Maybe String
    , rightCircle     :: Bool
    }

mkTurnstyle :: Pixel -> Pixel -> Pixel -> Pixel -> Turnstyle
mkTurnstyle u c r d = Turnstyle
    { leftPixel       = u
    , leftArrow       = NoArrow
    , leftLabel       = Nothing
    , leftCircle      = False
    , centerPixel     = c
    , centerArrow     = NoArrow
    , centerLabel     = Nothing
    , centerCircle    = False
    , frontPixel      = r
    , frontArrow      = NoArrow
    , frontLabel      = Nothing
    , frontCircle     = False
    , rightPixel      = d
    , rightArrow      = NoArrow
    , rightLabel      = Nothing
    , rightCircle     = False
    }

turnstyle :: Turnstyle -> Diagram B
turnstyle Turnstyle {..} =
    atop (mkArrow leftArrow (0.5 ^& (0.0)) unitY) $
    atop (mkArrow centerArrow ((0.5) ^& (-1.0)) unitX) $
    atop (mkArrow frontArrow (1.5 ^& (-1.0)) unitX) $
    atop (mkArrow rightArrow (0.5 ^& (-2.0)) unit_Y) $
    frame 1 $
    lw none $
    alignL (tile leftLabel leftCircle leftPixel)
    ===
    alignL (tile centerLabel centerCircle centerPixel |||
        tile frontLabel frontCircle frontPixel)
    ===
    alignL (tile rightLabel rightCircle rightPixel)
  where
    mkArrow NoArrow _ _ = mempty
    mkArrow Arrow p v =
        arrowAt' (with & headLength .~ 20) p v
    mkArrow DashedArrow p v =
        arrowAt' (with & headLength .~ 20) p v # dashingL [0.1, 0.1] 0

    tile label circ color =
        (case label of
            Nothing -> mempty
            Just txt -> translateY (-1/8) $ scale (2/3) (text txt)) `atop`
        (if circ then lw none $ fc black $ circle 0.1 else mempty) `atop`
        fc (colorToStyle color) (square 1)

main :: IO ()
main = do
    renderSVG "spec/enter.svg" (mkHeight 400) $
        let enter = turnstyle (mkTurnstyle A B C D)
                {centerArrow = Arrow, centerCircle = True} in
        alignT enter |||
        translateY (-1) (alignT (rotateBy (3 / 4) enter)) |||
        alignT (rotateBy (2 / 4) enter) |||
        alignT (rotateBy (1 / 4) enter)
    renderSVG "spec/label.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A B C D) {centerArrow = Arrow, centerCircle = True} |||
        turnstyle (mkTurnstyle A B C D)
            {leftLabel = Just "L", centerLabel = Just "C", frontLabel = Just "F", rightLabel = Just "R"}
    renderSVG "spec/app.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A B A C) {leftArrow = Arrow, leftCircle = True, frontArrow = DashedArrow, frontCircle = True} |||
        turnstyle (mkTurnstyle A B C A) {leftArrow = Arrow, leftCircle = True, rightArrow = DashedArrow, rightCircle = True} |||
        turnstyle (mkTurnstyle B C A A) {frontArrow = Arrow, frontCircle = True, rightArrow = DashedArrow, rightCircle = True}
    renderSVG "spec/lam.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A A B C) {leftArrow = Arrow, leftCircle = True} |||
        turnstyle (mkTurnstyle B A C A) {rightArrow = Arrow, rightCircle = True}
    renderSVG "spec/var.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle B A B B) {centerCircle = True} |||
        turnstyle (mkTurnstyle B B A B) {frontCircle = True}
    renderSVG "spec/symbol.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A B C D)
            {leftLabel = Just "L", frontLabel = Just "F", rightLabel = Just "R"}
    renderSVG "spec/id.svg" (mkHeight 400) $
        (turnstyle (mkTurnstyle A A A A) {frontArrow = Arrow, frontCircle = True} |||
            turnstyle (mkTurnstyle A A A B) {frontArrow = Arrow, frontCircle = True} |||
            turnstyle (mkTurnstyle B A A A) {frontArrow = Arrow, frontCircle = True})
        ===
        (turnstyle (mkTurnstyle B A A C) {frontArrow = Arrow, frontCircle = True} |||
            turnstyle (mkTurnstyle B A A B) {frontArrow = Arrow, frontCircle = True} |||
            turnstyle (mkTurnstyle A A B B) {leftArrow = Arrow, leftCircle = True} |||
            turnstyle (mkTurnstyle B A B A) {rightArrow = Arrow, rightCircle = True})

    renderSVG "spec/all.svg" (mkHeight 400) $
        (turnstyle (mkTurnstyle A A A A) |||
            turnstyle (mkTurnstyle A A A B) |||
            turnstyle (mkTurnstyle A A B A) |||
            turnstyle (mkTurnstyle A A B B) |||
            turnstyle (mkTurnstyle A A B C)) ===
        (turnstyle (mkTurnstyle A B A A) |||
            turnstyle (mkTurnstyle A B A B) |||
            turnstyle (mkTurnstyle A B A C) |||
            turnstyle (mkTurnstyle A B B A) |||
            turnstyle (mkTurnstyle A B B B)) ===
        (turnstyle (mkTurnstyle A B B C) |||
            turnstyle (mkTurnstyle A B C A) |||
            turnstyle (mkTurnstyle A B C B) |||
            turnstyle (mkTurnstyle A B C C) |||
            turnstyle (mkTurnstyle A B C D))
