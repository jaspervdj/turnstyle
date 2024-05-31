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

data Arrow = NoArrow | Arrow | DashedArrow deriving (Eq, Show)

data Turnstyle = Turnstyle
    { leftPixel       :: Pixel
    , leftArrow       :: Arrow
    , leftLabel       :: Maybe String
    , centerPixel     :: Pixel
    , centerArrow     :: Arrow
    , centerBackArrow :: Arrow
    , centerLabel     :: Maybe String
    , frontPixel      :: Pixel
    , frontArrow      :: Arrow
    , frontBackArrow  :: Arrow
    , frontLabel      :: Maybe String
    , rightPixel      :: Pixel
    , rightArrow      :: Arrow
    , rightLabel      :: Maybe String
    }

mkTurnstyle :: Pixel -> Pixel -> Pixel -> Pixel -> Turnstyle
mkTurnstyle u c r d = Turnstyle
    { leftPixel       = u
    , leftArrow       = NoArrow
    , leftLabel       = Nothing
    , centerPixel     = c
    , centerArrow     = NoArrow
    , centerBackArrow = NoArrow
    , centerLabel     = Nothing
    , frontPixel      = r
    , frontArrow      = NoArrow
    , frontBackArrow  = NoArrow
    , frontLabel      = Nothing
    , rightPixel      = d
    , rightArrow      = NoArrow
    , rightLabel      = Nothing
    }

turnstyle :: Turnstyle -> Diagram B
turnstyle Turnstyle {..} =
    atop (mkArrow leftArrow (0.5 ^& (0.0)) unitY) $
    atop (mkArrow centerArrow ((0.5) ^& (-1.0)) unitX) $
    atop (mkArrow centerBackArrow ((-0.5) ^& (-1.0)) unitX) $
    atop (mkArrow frontArrow (1.5 ^& (-1.0)) unitX) $
    atop (mkArrow frontBackArrow (2.5 ^& (-1.0)) unit_X) $
    atop (mkArrow rightArrow (0.5 ^& (-2.0)) unit_Y) $
    frame 1 $
    lw none $
    alignL (label leftLabel `atop` fc (colorToStyle leftPixel) square 1)
    ===
    alignL ((label centerLabel `atop` fc (colorToStyle centerPixel) (square 1)) |||
        (label frontLabel `atop` fc (colorToStyle frontPixel) (square 1)))
    ===
    alignL (label rightLabel `atop` fc (colorToStyle rightPixel) (square 1))
  where
    mkArrow NoArrow _ _ = mempty
    mkArrow Arrow p v =
        arrowAt' (with & headLength .~ large) p v
    mkArrow DashedArrow p v =
        arrowAt' (with & headLength .~ large) p v # dashingL [0.1, 0.1] 0

    label Nothing    = mempty
    label (Just txt) = translateY (-1/8) $ scale (2/3) (text txt)

initialization :: Diagram B
initialization =
    frame 1 $
    atop (arrowAt' (with & headLength .~ large) (0.5 ^& 0.0) (2.0 ^& 0.0)) $
    atop (arrowAt' (with & headLength .~ large) (0.5 ^& 0.0) unit_Y) $
    lw none $
    alignL (
        fc mistyrose (square 1) |||
        fc mistyrose (square 1) |||
        fc lightsteelblue (square 1) |||
        fc mistyrose (square 1))
    ===
    alignL (fc coral (square 1) |||
        fc mistyrose (square 1) |||
        fc lightsteelblue (square 1) |||
        fc mistyrose (square 1))
    ===
    alignL (fc mistyrose (square 1) |||
        fc lightsteelblue (square 1) |||
        fc mistyrose (square 1) |||
        fc mistyrose (square 1))

main :: IO ()
main = do
    renderSVG "spec/init.svg" (mkHeight 400) initialization
    renderSVG "spec/enter.svg" (mkHeight 400) $
        let enter = turnstyle (mkTurnstyle A B C D) {centerArrow = Arrow} in
        alignT enter |||
        translateY (-1) (alignT (rotateBy (3 / 4) enter)) |||
        alignT (rotateBy (2 / 4) enter) |||
        alignT (rotateBy (1 / 4) enter)
    renderSVG "spec/label.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A B C D) {centerArrow = Arrow} |||
        turnstyle (mkTurnstyle A B C D)
            {leftLabel = Just "L", centerLabel = Just "C", frontLabel = Just "F", rightLabel = Just "R"}
    renderSVG "spec/app.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A B A C) {leftArrow = Arrow, frontArrow = DashedArrow} |||
        turnstyle (mkTurnstyle A B C A) {leftArrow = Arrow, rightArrow = DashedArrow} |||
        turnstyle (mkTurnstyle B C A A) {frontArrow = Arrow, rightArrow = DashedArrow}
    renderSVG "spec/lam.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A A B C) {leftArrow = Arrow} |||
        turnstyle (mkTurnstyle B A C A) {rightArrow = Arrow}
    renderSVG "spec/var.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle B A B B) {centerBackArrow = Arrow} |||
        turnstyle (mkTurnstyle B B A B) {frontBackArrow = Arrow}
    renderSVG "spec/symbol.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A B C D) {leftArrow = Arrow, frontArrow = Arrow, rightArrow = Arrow}
    renderSVG "spec/id.svg" (mkHeight 400) $
        (turnstyle (mkTurnstyle A A A A) {frontArrow = Arrow} |||
            turnstyle (mkTurnstyle A A A B) {frontArrow = Arrow} |||
            turnstyle (mkTurnstyle B A A A) {frontArrow = Arrow})
        ===
        (turnstyle (mkTurnstyle B A A C) {frontArrow = Arrow} |||
            turnstyle (mkTurnstyle A A B B) {leftArrow = Arrow} |||
            turnstyle (mkTurnstyle B A B A) {rightArrow = Arrow})
