{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Turnstyle.Compile.Shape
    ( ColorConstraint (..)
    , Shape (..)
    , Layout (..)
    , LamLayout (..)
    , defaultLayout
    , exprToShape
    ) where

import qualified Data.Map       as M
import           Data.Void      (Void, absurd)
import           Turnstyle.Expr
import           Turnstyle.Prim
import           Turnstyle.TwoD

data ColorConstraint p
    = Eq p p
    | NotEq p p
    deriving (Foldable, Functor, Show)

data Shape = Shape
    { sWidth       :: Int
    , sHeight      :: Int
    , sEntrance    :: Int  -- Pixels from top where we "enter" the shape.
    , sConstraints :: [ColorConstraint Pos]
    } deriving (Show)

newtype Transform = Transform {unTransform :: Shape -> (Shape, Pos -> Pos)}

instance Semigroup Transform where
    Transform f <> Transform g = Transform $ \shape0 ->
        let (shape1, posMap1) = g shape0
            (shape2, posMap2) = f shape1 in
        (shape2, posMap1 . posMap2)

rotateShapeLeft :: Transform
rotateShapeLeft = Transform $ \s ->
    let rot    (Pos x y) = Pos y (sWidth s - x - 1)
        revPos (Pos x y) = Pos (sWidth s - y - 1) x in
    ( Shape
        { sWidth       = sHeight s
        , sHeight      = sWidth s
        , sEntrance    = sEntrance s
        , sConstraints = fmap rot <$> sConstraints s
        }
    , revPos
    )

offsetShape :: Int -> Int -> Transform
offsetShape dx dy = Transform $ \s ->
    let fwd (Pos x y) = Pos (x + dx) (y + dy)
        bwd (Pos x y) = Pos (x - dx) (y - dy) in
    (s {sConstraints = fmap fwd <$> sConstraints s}, bwd)

data Layout
    = LamLayout LamLayout
    | NoLayout
    deriving (Eq, Show)

data LamLayout
    = LamLeft
    | LamRight
    deriving (Eq, Show)

defaultLayout :: Expr ann e v -> Expr Layout e v
defaultLayout (App _ f x) = App NoLayout (defaultLayout f) (defaultLayout x)
defaultLayout (Lam _ v b) = Lam (LamLayout LamLeft) v (defaultLayout b)
defaultLayout (Var _ v) = Var NoLayout v
defaultLayout (Prim _ p) = Prim NoLayout p
defaultLayout (Lit _ l) = Lit NoLayout l
defaultLayout (Err _ e) = Err NoLayout e

exprToShape :: (Show v, Ord v) => Expr Layout Void v -> Shape
exprToShape = exprToShape' M.empty

exprToShape' :: (Ord v, Show v) => M.Map v Pos -> Expr Layout Void v -> Shape
exprToShape' ctx expr = case expr of
    App _ lhs rhs -> Shape
        { sWidth       = max 3 (max (sWidth lhsShape + offsetL) (sWidth rhsShape + offsetR))
        , sHeight      = sHeight lhsShape + 3 + sHeight rhsShape
        , sEntrance    = sHeight lhsShape + 1
        , sConstraints =
            -- Turnstyle shape
            [ NotEq appL appC, NotEq appL appF, Eq appL appR
            , NotEq appC appF
            ] ++
            -- Tunnel
            [Eq (move x R enterL) (move x R enterR) | x <- [0 .. entrance - 1]] ++
            [Eq appC (move x R enterC) | x <- [0 .. entrance - 1]] ++
            [Eq (move 1 L appR) (move 1 R appL)] ++
            [Eq (move 1 L appR) (move 1 R appR)] ++
            -- Connect to LHS
            [Eq appL (move 1 U appL)] ++
            -- Connect to RHS
            [Eq appR (move 1 D appR)] ++
            -- LHS
            sConstraints lhsShape ++
            -- RHS
            sConstraints rhsShape
        }
      where
        lhsContext            = lhsCtxMap <$> ctx
        (lhsShape, lhsCtxMap) = unTransform
            (offsetShape offsetL 0 <> rotateShapeLeft)
            (exprToShape' lhsContext lhs)

        rhsContext            = rhsCtxMap <$> ctx
        (rhsShape, rhsCtxMap) = unTransform
            (offsetShape offsetR (sHeight lhsShape + 3) <>
                rotateShapeLeft <>
                rotateShapeLeft <>
                rotateShapeLeft)
            (exprToShape' rhsContext rhs)

        entranceL = sEntrance lhsShape
        entranceR = sWidth rhsShape - sEntrance rhsShape - 1
        entrance  = max entranceL entranceR
        offsetL   = entrance - entranceL
        offsetR   = entrance - entranceR

        enterL = move 1 U enterC
        enterC = Pos 0 (sHeight lhsShape + 1)
        enterF = move 1 R enterC
        enterR = move 1 D enterC

        appL = move entrance R enterL
        appR = move entrance R enterR
        appF = move entrance R enterF
        appC = move entrance R enterC

    Lam layout v body -> case layout of
        LamLayout LamLeft -> Shape
            { sWidth       = max 3 (sWidth bodyShape)
            , sHeight      = sHeight bodyShape + 3
            , sEntrance    = entrance
            , sConstraints =
                -- Turnstyle shape
                [ Eq lamL lamC, NotEq lamL lamF, NotEq lamL lamR
                , NotEq lamF lamR
                ] ++
                -- Tunnel
                [Eq (move x R enterR) (move x R enterL)   | x <- [0 .. bodyEntrance - 1]] ++
                [Eq lamC (move x R enterC) | x <- [0 .. bodyEntrance - 1]] ++
                [Eq (move 1 L lamL) (move 1 R lamL)] ++
                -- Connect to body
                [Eq lamL (move 1 U lamL)] ++
                -- Body
                sConstraints bodyShape ++
                -- Variable uniqueness
                (case M.lookup v ctx of
                    Just p  -> [Eq lamR p]
                    Nothing -> [NotEq lamR q | (_, q) <- M.toList ctx])
            }
        LamLayout LamRight -> Shape
            { sWidth       = max 3 (sWidth bodyShape)
            , sHeight      = sHeight bodyShape + 3
            , sEntrance    = entrance
            , sConstraints =
                -- Turnstyle shape
                [ Eq lamR lamC, NotEq lamR lamF, NotEq lamR lamL
                , NotEq lamF lamL
                ] ++
                -- Tunnel
                [Eq (move x R enterR) (move x R enterL)   | x <- [0 .. bodyEntrance - 1]] ++
                [Eq lamC (move x R enterC) | x <- [0 .. bodyEntrance - 1]] ++
                [Eq (move 1 L lamR) (move 1 R lamR)] ++
                -- Connect to body
                [Eq lamR (move 1 D lamR)] ++
                -- Body
                sConstraints bodyShape ++
                -- Variable uniqueness
                (case M.lookup v ctx of
                    Just p  -> [Eq lamL p]
                    Nothing -> [NotEq lamL q | (_, q) <- M.toList ctx])
            }
      where
        bodyContext =
            M.insert v (Pos (-3) (sEntrance bodyShape)) $
            fmap mapCtx $ ctx

        (bodyShape, mapCtx) = unTransform
            (case layout of
                LamLayout LamLeft  -> rotateShapeLeft
                LamLayout LamRight -> offsetShape 0 3 <>
                    rotateShapeLeft <> rotateShapeLeft <> rotateShapeLeft)
            (exprToShape' bodyContext body)

        lamL = move bodyEntrance R enterL
        lamR = move bodyEntrance R enterR
        lamF = move bodyEntrance R enterF
        lamC = move bodyEntrance R enterC

        entrance = case layout of
            LamLayout LamLeft -> sHeight bodyShape + 1
            LamLayout LamRight -> 1
        bodyEntrance = case layout of
            LamLayout LamLeft -> sEntrance bodyShape
            LamLayout LamRight -> sWidth bodyShape - sEntrance bodyShape - 1

        enterL = move 1 U enterC
        enterC = Pos 0 entrance
        enterF = move 1 R enterC
        enterR = move 1 D enterC

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
      where
        left   = move 1 U center
        center = Pos 0 1
        front  = move 1 R center
        right  = move 1 D center

    Prim _ prim -> Shape
        { sWidth       = max 2 (max (frontArea + 1) rightArea)
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

        left   = move 1 U center
        center = Pos 0 1
        front  = move 1 R center
        right  = move 1 D center

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
            [ NotEq right p | p <- neighbors right ] ++
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

        left   = move 1 U center
        center = Pos 0 1
        front  = move 1 R center
        right  = move 1 D center


    Err _ void -> absurd void
