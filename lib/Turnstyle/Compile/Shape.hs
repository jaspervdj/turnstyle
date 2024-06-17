{-# LANGUAGE DeriveFunctor #-}
module Turnstyle.Compile.Shape
    ( ColorConstraint (..)
    , Shape (..)
    , exprToShape
    ) where

import qualified Data.Map       as M
import           Data.Void      (Void, absurd)
import           Debug.Trace
import           Turnstyle.Expr
import           Turnstyle.Prim
import           Turnstyle.TwoD

data ColorConstraint p
    = Eq p p
    | NotEq p p
    deriving (Functor, Show)

data Shape = Shape
    { sWidth       :: Int
    , sHeight      :: Int
    , sEntrance    :: Int  -- Pixels from top where we "enter" the shape.
    , sConstraints :: [ColorConstraint Pos]
    } deriving (Show)

rotateShapeLeft :: Int -> Int -> (Shape, M.Map v Pos) -> (Shape, M.Map v Pos)
rotateShapeLeft entrance offsetY (s, ctx) =
    ( Shape
        { sWidth       = sHeight s
        , sHeight      = sWidth s
        , sEntrance    = sEntrance s
        , sConstraints = fmap rot <$> sConstraints s
        }
    , revRot <$> ctx
    )
  where
    revRot (Pos x y) = Pos (y - offsetY) (-x - entrance)
    rot    (Pos x y) = Pos (-y - entrance) (x + offsetY)

exprToShape :: Ord v => Expr ann Void v -> Shape
exprToShape = exprToShape' M.empty

exprToShape' :: Ord v => M.Map v Pos -> Expr ann Void v -> Shape
exprToShape' ctx expr = case expr of

    Lam _ v body -> Shape
        { sWidth       = max 3 (sWidth bodyShape)
        , sHeight      = sHeight bodyShape + 3
        , sEntrance    = sHeight bodyShape + 1
        , sConstraints =
            -- Turnstyle shape
            [ Eq lamL lamC, NotEq lamL lamF, NotEq lamL lamR
            , NotEq lamF lamR
            ]
            -- sConstraints bodyShape
        }
      where
        (bodyShape, bodyCtx) = rotateShapeLeft (sEntrance bodyShape) 1
            (exprToShape' (M.insert v right bodyCtx) body, ctx)

        lamL = move (sEntrance bodyShape) R left
        lamR = move (sEntrance bodyShape) R right
        lamF = move (sEntrance bodyShape) R front
        lamC = move (sEntrance bodyShape) R center

    Var _ v -> case M.lookup v ctx of
        Nothing  -> error "exprToShape: unbound variable"
        Just pos -> Shape
            { sWidth       = 2
            , sHeight      = 3
            , sEntrance    = 1
            , sConstraints =
                -- Turnstyle shape
                [ Eq left center, NotEq left front, Eq left right
                ] ++
                -- Variable
                [ Eq front pos
                ]
            }

    Prim _ prim -> Shape
        { sWidth       = max 2 (max (frontArea + 1) rightArea)
        , sHeight      = 3
        , sEntrance    = 1
        , sConstraints =
            trace ("frontArea: " ++ show frontArea) $
            trace ("rightArea: " ++ show rightArea) $
            -- Turnstyle shape
            [ NotEq left center, NotEq left front, NotEq left right
            , NotEq center front, NotEq center right
            , NotEq front right
            ] ++
            -- Left pixel should have area 1
            [ NotEq left p | p <- neighbors left ] ++
            -- Front "mode" part should be one color
            [ Eq front e | e <- frontExtension ] ++
            -- Areas around the front "mode" part need to be different
            [ NotEq front (move 1 U e) | e <- frontExtension ] ++
            [ NotEq front (move 1 D e) | e <- frontExtension ] ++
            [ NotEq front (move frontArea R front) ] ++
            -- Right "opcode" part should be one color
            [ Eq right e | e <- rightExtension ] ++
            -- Areas around the right "opcode" part need to be different
            [ NotEq right (move 1 U e) | e <- rightExtension ] ++
            [ NotEq right (move 1 D e) | e <- rightExtension ] ++
            [ NotEq right (move rightArea R right) ] ++
            [ NotEq right (move 1 L right) ]
        }
      where
        frontExtension = [move i R front | i <- [0 .. frontArea - 1]]
        rightExtension = [move i R right | i <- [0 .. rightArea - 1]]
        (opcode, mode) = encodePrim prim
        rightArea = opcode + 1
        frontArea =
            if mode < rightArea then rightArea - mode else rightArea + mode

    Lit _ n -> Shape
        { sWidth       = 1 + n
        , sHeight      = 3
        , sEntrance    = 1
        , sConstraints =
            -- Turnstyle shape
            [ NotEq left center, NotEq left front, NotEq left right
            , NotEq center front, NotEq center right
            , NotEq front right
            ] ++
            -- Left pixel should have area 1
            [ NotEq left p | p <- neighbors left ] ++
            -- Right pixel should have area 1
            [ NotEq right p | p <- neighbors left ] ++
            -- Front "lit" part should be one color
            [ Eq front e | e <- frontExtension ] ++
            -- Areas around the front "lit" part need to be different
            [ NotEq front (move 1 U e) | e <- frontExtension ] ++
            [ NotEq front (move 1 D e) | e <- frontExtension ] ++
            [ NotEq front (move n R front) ]
        }
      where
        -- The extension for a literal
        frontExtension = [move i R center | i <- [1 .. n]]

    Err _ void -> absurd void
  where
    left   = move 1 U center
    center = Pos 0 0
    front  = move 1 R center
    right  = move 1 D center

