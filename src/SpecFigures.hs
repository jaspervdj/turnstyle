{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies              #-}
import           Diagrams.Backend.SVG
import           Diagrams.Prelude

data Pixel = A | B | C | D deriving (Eq, Show)

colorToStyle :: Pixel -> Colour Double
colorToStyle A = sRGB24read "#eeb480"
colorToStyle B = sRGB24read "#b5decc"
colorToStyle C = sRGB24read "#ffdd00"
colorToStyle D = sRGB24read "#b2b73e"

data Arrow
    = NoArrow
    | Arrow
    | DashedArrow
    deriving (Eq, Show)

data Turnstyle = Turnstyle
    { frameSize       :: Double
    , leftPixel       :: Pixel
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
    { frameSize       = 1
    , leftPixel       = u
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
    atop (moveTo (0.5 ^& (0.0))  $ rotateBy (0/4) $ mkArrow leftArrow) $
    atop (moveTo (0.5 ^& (-1.0)) $ rotateBy (3/4) $ mkArrow centerArrow) $
    atop (moveTo (1.5 ^& (-1.0)) $ rotateBy (3/4) $ mkArrow frontArrow) $
    atop (moveTo (0.5 ^& (-2.0)) $ rotateBy (2/4) $ mkArrow rightArrow) $
    frame frameSize $
    lw none $
    alignL (tile leftLabel leftCircle leftPixel)
    ===
    alignL (tile centerLabel centerCircle centerPixel |||
        tile frontLabel frontCircle frontPixel)
    ===
    alignL (tile rightLabel rightCircle rightPixel)
  where
    mkArrow NoArrow = mempty
    mkArrow Arrow =
        fromVertices [P zero, P unitY] <>
        (moveTo (P unitY) $ fc black $ triangle 0.2)
    mkArrow DashedArrow =
        dashingL [0.1, 0.1] 0 fromVertices [P zero, P unitY] <>
        (moveTo (P unitY) $ fc black $ triangle 0.2)

    tile label circ color =
        (case label of
            Nothing -> mempty
            Just txt -> translateY (-1/8) $ scale (2/3) (text txt)) `atop`
        (if circ then lw none $ fc black $ circle 0.1 else mempty) `atop`
        fc (colorToStyle color) (square 1)

appSpec :: Diagram B
appSpec =
    turnstyle (mkTurnstyle A B A C) {leftArrow = Arrow, leftCircle = True, frontArrow = DashedArrow, frontCircle = True} |||
    turnstyle (mkTurnstyle A B C A) {leftArrow = Arrow, leftCircle = True, rightArrow = DashedArrow, rightCircle = True} |||
    turnstyle (mkTurnstyle B C A A) {frontArrow = Arrow, frontCircle = True, rightArrow = DashedArrow, rightCircle = True}
lamSpec :: Diagram B
lamSpec =
    turnstyle (mkTurnstyle C A B A) {rightArrow = Arrow, rightCircle = True, leftLabel = Just "L"} |||
    turnstyle (mkTurnstyle B A A C) {frontArrow = Arrow, frontCircle = True, centerLabel = Just "C"} |||
    turnstyle (mkTurnstyle A A B C) {leftArrow = Arrow, leftCircle = True, rightLabel = Just "R"}
varSpec :: Diagram B
varSpec =
    turnstyle (mkTurnstyle A B B B) {leftLabel = Just "L"} |||
    turnstyle (mkTurnstyle B A B B) {centerLabel = Just "C"} |||
    turnstyle (mkTurnstyle B B A B) {frontLabel = Just "F"} |||
    turnstyle (mkTurnstyle B B B A) {rightLabel = Just "R"}
symbolSpec :: Diagram B
symbolSpec =
    turnstyle (mkTurnstyle A B C D)
        {leftLabel = Just "L", frontLabel = Just "F", rightLabel = Just "R"}
idSpec :: Diagram B
idSpec =
    turnstyle (mkTurnstyle A A A A) {frontArrow = Arrow, frontCircle = True} |||
    turnstyle (mkTurnstyle B A A B) {frontArrow = Arrow, frontCircle = True} |||
    turnstyle (mkTurnstyle A A B B) {leftArrow = Arrow, leftCircle = True} |||
    turnstyle (mkTurnstyle B A B A) {rightArrow = Arrow, rightCircle = True}

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
    renderSVG "spec/app.svg" (mkHeight 400) appSpec
    renderSVG "spec/lam.svg" (mkHeight 400) lamSpec
    renderSVG "spec/var.svg" (mkHeight 400) varSpec
    renderSVG "spec/symbol.svg" (mkHeight 400) symbolSpec
    renderSVG "spec/id.svg" (mkHeight 400) idSpec

    renderSVG "spec/cheatsheet.svg" (mkHeight 800) $
        ((moveTo (0 ^& (-1)) $ frame 2 $ italic $ text "x") <>
            (moveTo (0 ^& (-6)) $ frame 2 $ italic $ text "(λv.e)") <>
            (moveTo (0 ^& (-11)) $ frame 2 $ italic $ text "(f x)") <>
            (moveTo (0 ^& (-16)) $ frame 2 $ font "monospace" $ text "sym") <>
            (moveTo (0 ^& (-21)) $ frame 2 $ italic $ text "I")) |||
        (varSpec ===
            lamSpec ===
            appSpec ===
            (atop symbolSpec $
                (moveTo (4 ^& (-0.5)) $ italic $ scale (2/3) $
                    alignedText 0 0.5 "A(L) ≥ A(R) ⇒ N(A(F))") <>
                (moveTo (4 ^& (-1.5)) $ italic $ scale (2/3) $
                    alignedText 0 0.5 "else ⇒ P(A(R)-A(L), |A(F)-A(R)|)")) ===
            idSpec)

    renderSVG "website/one.svg" (mkHeight 400) $
        turnstyle (mkTurnstyle A B C D) {frameSize = 0}
