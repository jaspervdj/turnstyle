{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE Rank2Types     #-}
module Turnstyle.Compile.Shape
    ( ColorConstraint (..)
    , Shape (..)
    , exprToShape
    ) where

import qualified Codec.Picture                as JP
import           Control.Monad                (guard)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           Turnstyle.Compile.Constraint
import           Turnstyle.Compile.Expr
import           Turnstyle.Compile.Recompile
import qualified Turnstyle.Image              as Image
import           Turnstyle.JuicyPixels        (JuicyPixels)
import           Turnstyle.Prim
import           Turnstyle.TwoD

data Shape ann = Shape
    { sWidth       :: Int
    , sHeight      :: Int
    , sEntrance    :: Int  -- Pixels from top where we "enter" the shape.
    , sConstraints :: [ColorConstraint (Image.Pixel JuicyPixels) Pos]
    , sSourceMap   :: [(Pos, Dir, ann)]
    } deriving (Show)

newtype Transform = Transform
    { unTransform :: forall ann. Shape ann -> (Shape ann, Pos -> Pos)
    }

instance Semigroup Transform where
    Transform f <> Transform g = Transform $ \shape0 ->
        let (shape1, posMap1) = g shape0
            (shape2, posMap2) = f shape1 in
        (shape2, posMap1 . posMap2)

data Context v = Context
    { cVars :: M.Map v Pos
    , cPath :: Maybe v
    }

transformContext :: (Pos -> Pos) -> Context v -> Context v
transformContext f ctx = ctx
    { cVars = fmap f (cVars ctx)
    }

rotateShapeLeft :: Transform
rotateShapeLeft = Transform $ \s ->
    let rot    (Pos x y) = Pos y (sWidth s - x - 1)
        revPos (Pos x y) = Pos (sWidth s - y - 1) x in
    ( Shape
        { sWidth       = sHeight s
        , sHeight      = sWidth s
        , sEntrance    = sEntrance s
        , sSourceMap   = do
            (pos, dir, ann) <- sSourceMap s
            pure (rot pos, rotateLeft dir, ann)
        , sConstraints = fmap rot <$> sConstraints s
        }
    , revPos
    )

offsetShape :: Int -> Int -> Transform
offsetShape dx dy = Transform $ \s ->
    let fwd (Pos x y) = Pos (x + dx) (y + dy)
        bwd (Pos x y) = Pos (x - dx) (y - dy) in
    ( s { sSourceMap = [(fwd p, d, a) | (p, d, a) <- sSourceMap s]
        , sConstraints = fmap fwd <$> sConstraints s
        }
    , bwd
    )

-- | A 'Shape' stores it entrance.  This is not necessarily in the center of the
-- image on the left side, since doing that would introduce significant negative
-- space for every sub-expression.  Instead, we do this centering only once, for
-- the top-level expression of the program.
topLevelShape :: Shape ann -> Shape ann
topLevelShape s = fst $ unTransform (offsetShape offsetX offsetY) $ s
    { sEntrance = sEntrance s + offsetY
    , sHeight   = spacingHeight * 2 + 1
    }
  where
    topHeight     = sEntrance s
    bottomHeight  = sHeight s - sEntrance s - 1
    spacingHeight = max topHeight bottomHeight
    offsetX       = 0
    offsetY       = spacingHeight - sEntrance s

exprToShape :: Ord v => Expr ann v -> Shape ann
exprToShape = topLevelShape . toShape (Context M.empty Nothing)

toShape :: Ord v => Context v -> Expr ann v -> Shape ann
toShape ctx expr = case expr of
    App ann AppLeftRight lhs rhs -> Shape
        { sWidth       = max 3 (max (sWidth lhsShape + offsetL) (sWidth rhsShape + offsetR))
        , sHeight      = sHeight lhsShape + 3 + sHeight rhsShape
        , sEntrance    = sHeight lhsShape + 1
        , sSourceMap   =
            (appC, R, ann) : sSourceMap lhsShape ++ sSourceMap rhsShape
        , sConstraints =
            -- Turnstyle shape
            [ NotEq appL appC, NotEq appL appF, Eq appL appR
            , NotEq appC appF
            ] ++
            -- Tunnel
            [Eq (move x R enterL) (move x R enterR) | x <- [0 .. entrance - 1]] ++
            [Eq appC (move x R enterC) | x <- [0 .. entrance - 1]] ++
            [Eq (move 1 L appR) (move 1 R appL)] ++  -- Fishy
            [Eq (move 1 L appR) (move 1 R appR)] ++  -- Fishy
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
        lhsContext = (transformContext lhsCtxMap ctx) {cPath = Nothing}
        (lhsShape, lhsCtxMap) = unTransform
            (offsetShape offsetL 0 <> rotateShapeLeft)
            (toShape lhsContext lhs)

        rhsContext = (transformContext rhsCtxMap ctx) {cPath = Nothing}
        (rhsShape, rhsCtxMap) = unTransform
            (offsetShape offsetR (sHeight lhsShape + 3) <>
                rotateShapeLeft <>
                rotateShapeLeft <>
                rotateShapeLeft)
            (toShape rhsContext rhs)

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

    App ann AppLeftFront lhs rhs -> Shape
        { sWidth       = max 3 (sWidth lhsShape) + sWidth rhsShape
        , sHeight      = max (sHeight lhsShape + 3) (offsetR + sHeight rhsShape)
        , sEntrance    = entrance
        , sSourceMap   =
            (appC, R, ann) : sSourceMap lhsShape ++ sSourceMap rhsShape
        , sConstraints =
            -- Turnstyle shape
            [ NotEq appL appC, Eq appL appF, NotEq appL appR
            , NotEq appC appR
            ] ++
            -- Tunnel from entrance to app
            [Eq (move x R enterL) (move x R enterR) | x <- [0 .. entranceL - 1]] ++
            [Eq appC (move x R enterC) | x <- [0 .. entranceL - 1]] ++
            [Eq (move 1 L appL) (move 1 R appL)] ++
            -- Connect to LHS
            [Eq appL (move 1 U appL)] ++
            -- Tunnel to RHS
            [Eq (move x R enterL) (move x R enterR) | x <- [entranceL + 1 .. rhsX - 1]] ++
            [Eq appF (move x R enterC) | x <- [entranceL + 1 .. rhsX - 1]] ++
            -- Connect to RHS
            [Eq appF (move rhsX R enterC)] ++
            -- LHS
            sConstraints lhsShape ++
            -- RHS
            sConstraints rhsShape
        }
      where
        lhsContext = (transformContext lhsCtxMap ctx) {cPath = Nothing}
        (lhsShape, lhsCtxMap) = unTransform
            (offsetShape 0 (entrance - sHeight lhsShape - 1) <> rotateShapeLeft)
            (toShape lhsContext lhs)

        rhsContext = (transformContext rhsCtxMap ctx) {cPath = Nothing}
        (rhsShape, rhsCtxMap) = unTransform
            (offsetShape (sWidth lhsShape) offsetR)
            (toShape rhsContext rhs)

        rhsX = sWidth lhsShape

        entrance  = max (sHeight lhsShape + 1) (sEntrance rhsShape)
        entranceL = sEntrance lhsShape
        offsetR   = entrance - sEntrance rhsShape

        enterL = move 1 U enterC
        enterC = Pos 0 entrance
        enterF = move 1 R enterC
        enterR = move 1 D enterC

        appL = move entranceL R enterL
        appR = move entranceL R enterR
        appF = move entranceL R enterF
        appC = move entranceL R enterC

    App ann AppFrontRight lhs rhs -> Shape
        { sWidth       = max 3 (sWidth rhsShape) + sWidth lhsShape
        , sHeight      = max (sHeight lhsShape) (entrance + 2 + sHeight rhsShape)
        , sEntrance    = entrance
        , sSourceMap   =
            (appC, R, ann) : sSourceMap lhsShape ++ sSourceMap rhsShape
        , sConstraints =
            -- Turnstyle shape
            [ Eq appR appF, NotEq appR appC, NotEq appR appL
            , NotEq appC appL
            ] ++
            -- Tunnel from entrance to app
            [Eq (move x R enterL) (move x R enterR) | x <- [0 .. entranceR - 1]] ++
            [Eq appC (move x R enterC) | x <- [0 .. entranceR - 1]] ++
            [Eq (move 1 L appR) (move 1 R appR)] ++
            -- Connect to RHS
            [Eq appR (move 1 D appR)] ++
            -- Tunnel to LHS
            [Eq (move x R enterL) (move x R enterR) | x <- [entranceR + 1 .. lhsX - 1]] ++
            [Eq appF (move x R enterC) | x <- [entranceR + 1 .. lhsX - 1]] ++
            -- Connect to LHS
            [Eq appF (move lhsX R enterC)] ++
            -- LHS
            sConstraints lhsShape ++
            -- RHS
            sConstraints rhsShape
        }
      where
        lhsContext = (transformContext lhsCtxMap ctx) {cPath = Nothing}
        (lhsShape, lhsCtxMap) = unTransform
            (offsetShape lhsX 0)
            (toShape lhsContext lhs)

        rhsContext = (transformContext rhsCtxMap ctx) {cPath = Nothing}
        (rhsShape, rhsCtxMap) = unTransform
            (offsetShape 0 (entrance + 2) <>
                rotateShapeLeft <> rotateShapeLeft <> rotateShapeLeft)
            (toShape rhsContext rhs)

        entrance  = sEntrance lhsShape
        entranceR = sWidth rhsShape - sEntrance rhsShape - 1
        lhsX      = sWidth rhsShape

        enterL = move 1 U enterC
        enterC = Pos 0 entrance
        enterF = move 1 R enterC
        enterR = move 1 D enterC

        appL = move entranceR R enterL
        appR = move entranceR R enterR
        appF = move entranceR R enterF
        appC = move entranceR R enterC

    -- If we want to construct a lambda shape, we must check if the variable for
    -- the lambda is defined by the exact same region as the "path of Ids" shape
    -- that our program drawing.  As an example, consider `\x. \x. x`, where the
    -- first lambda has a front layout.  In such cases, we cannot pick a left or
    -- right layout for the nested lambda.  Doing so would mean assigning x to a
    -- color outside the path, inconsistent with x.
    Lam ann LamFront v body -> Shape
        { sWidth       = 1 + sWidth bodyShape
        , sHeight      = sHeight bodyShape
        , sEntrance    = entrance
        , sSourceMap   = (lamC, R, ann) : sSourceMap bodyShape
        , sConstraints =
            -- Turnstyle shape
            [ Eq lamC lamF, NotEq lamC lamL, NotEq lamC lamR
            , NotEq lamL lamR
            ] ++
            -- Body
            sConstraints bodyShape ++
            -- Variable uniqueness
            (case M.lookup v (cVars ctx) of
                Just p  -> [Eq lamC p]
                Nothing -> [NotEq lamC q | (_, q) <- M.toList (cVars ctx)])
        }
      where
        varPos = Pos 0 entrance

        bodyContext = ctx
            { cVars = M.insert v varPos $ cVars $ transformContext mapCtx ctx
            , cPath = Just v
            }

        (bodyShape, mapCtx) = unTransform
            (offsetShape 1 0)
            (toShape bodyContext body)

        entrance = sEntrance bodyShape

        lamL = move 1 U lamC
        lamC = Pos 0 entrance
        lamF = move 1 R lamC
        lamR = move 1 D lamC

    Lam ann LamLeft v body -> Shape
        { sWidth       = max 3 (sWidth bodyShape)
        , sHeight      = sHeight bodyShape + 3
        , sEntrance    = entrance
        , sSourceMap   = (lamC, R, ann) : sSourceMap bodyShape
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
            (case M.lookup v (cVars ctx) of
                Just p  -> [Eq lamR p]
                Nothing -> [NotEq lamR q | (_, q) <- M.toList (cVars ctx)])
        }
      where
        bodyContext = ctx
            { cVars = M.insert v (Pos (-3) (sEntrance bodyShape)) $
                cVars $ transformContext mapCtx ctx
            }
        (bodyShape, mapCtx) = unTransform rotateShapeLeft
            (toShape bodyContext body)

        lamL = move bodyEntrance R enterL
        lamR = move bodyEntrance R enterR
        lamF = move bodyEntrance R enterF
        lamC = move bodyEntrance R enterC

        entrance = sHeight bodyShape + 1
        bodyEntrance = sEntrance bodyShape

        enterL = move 1 U enterC
        enterC = Pos 0 entrance
        enterF = move 1 R enterC
        enterR = move 1 D enterC

    Lam ann LamRight v body -> Shape
        { sWidth       = max 3 (sWidth bodyShape)
        , sHeight      = sHeight bodyShape + 3
        , sEntrance    = entrance
        , sSourceMap   = (lamC, R, ann) : sSourceMap bodyShape
        , sConstraints =
            -- Turnstyle shape
            [ Eq lamR lamC, NotEq lamR lamF, NotEq lamR lamL
            , NotEq lamF lamL
            ] ++
            -- Tunnel
            [Eq (move x R enterR) (move x R enterL) | x <- [0 .. bodyEntrance - 1]] ++
            [Eq lamC (move x R enterC) | x <- [0 .. bodyEntrance - 1]] ++
            [Eq (move 1 L lamR) (move 1 R lamR)] ++
            -- Connect to body
            [Eq lamR (move 1 D lamR)] ++
            -- Body
            sConstraints bodyShape ++
            -- Variable uniqueness
            (case M.lookup v (cVars ctx) of
                Just p  -> [Eq lamL p]
                Nothing -> [NotEq lamL q | (_, q) <- M.toList (cVars ctx)])
        }
      where
        bodyContext = ctx
            { cVars = M.insert v (Pos (-3) (sEntrance bodyShape)) $
                cVars $ transformContext mapCtx $ ctx
            }

        (bodyShape, mapCtx) = unTransform
            (offsetShape 0 3 <>
                rotateShapeLeft <> rotateShapeLeft <> rotateShapeLeft)
            (toShape bodyContext body)

        lamL = move bodyEntrance R enterL
        lamR = move bodyEntrance R enterR
        lamF = move bodyEntrance R enterF
        lamC = move bodyEntrance R enterC

        entrance = 1
        bodyEntrance = sWidth bodyShape - sEntrance bodyShape - 1

        enterL = move 1 U enterC
        enterC = Pos 0 entrance
        enterF = move 1 R enterC
        enterR = move 1 D enterC

    Var ann VarCenter v -> case M.lookup v (cVars ctx) of
        Nothing  -> error "exprToShape: unbound variable"
        Just pos -> Shape
            { sWidth       = 2
            , sHeight      = 3
            , sEntrance    = 1
            , sSourceMap   = [(center, R, ann)]
            , sConstraints =
                -- Turnstyle shape
                [ Eq left front, Eq left right, NotEq left center
                ] ++
                -- Variable
                [ Eq center pos
                ]
            }
          where
            left   = move 1 U center
            center = Pos 0 1
            front  = move 1 R center
            right  = move 1 D center

    Var ann VarFront v -> case M.lookup v (cVars ctx) of
        Nothing  -> error "exprToShape: unbound variable"
        Just pos -> Shape
            { sWidth       = 2
            , sHeight      = 3
            , sEntrance    = 1
            , sSourceMap   = [(center, R, ann)]
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

    Prim ann prim -> Shape
        { sWidth       = max 2 (max (frontArea + 1) (max leftArea rightArea))
        , sHeight      = 3
        , sEntrance    = 1
        , sSourceMap   = [(center, R, ann)]
        , sConstraints =
            -- Turnstyle shape
            [ NotEq left center, NotEq left front, NotEq left right
            , NotEq center front, NotEq center right
            , NotEq front right
            ] ++
            -- Left part should be one color
            [ Eq left e | e <- leftExtension ] ++
            -- Areas around the left part need to be different
            [ NotEq left (move 1 U e) | e <- leftExtension ] ++
            [ NotEq left (move 1 D e) | e <- leftExtension ] ++
            [ NotEq left (move leftArea R left) ] ++
            [ NotEq left (move 1 L left) ] ++
            -- Front part should be one color
            [ Eq front e | e <- frontExtension ] ++
            -- Areas around the front part need to be different
            [ NotEq front (move 1 U e) | e <- frontExtension ] ++
            [ NotEq front (move 1 D e) | e <- frontExtension ] ++
            [ NotEq front (move frontArea R front) ] ++
            -- Right part should be one color
            [ Eq right e | e <- rightExtension ] ++
            -- Areas around the right "opcode" part need to be different
            [ NotEq right (move 1 U e) | e <- rightExtension ] ++
            [ NotEq right (move 1 D e) | e <- rightExtension ] ++
            [ NotEq right (move rightArea R right) ] ++
            [ NotEq right (move 1 L right) ]
        }
      where
        leftExtension  = [move i R left  | i <- [0 .. leftArea  - 1]]
        frontExtension = [move i R front | i <- [0 .. frontArea - 1]]
        rightExtension = [move i R right | i <- [0 .. rightArea - 1]]
        (modul, opcode) = encodePrim prim
        leftArea  = 2
        frontArea = modul
        rightArea = opcode

        left   = move 1 U center
        center = Pos 0 1
        front  = move 1 R center
        right  = move 1 D center

    Lit ann (LitLayout upH downH) n -> Shape
        { sWidth       = 1 + maximum [x | Pos x _ <- frontExtension]
        , sHeight      = 3 + max 0 (upH - 1) + max 0 (downH - 1)
        , sEntrance    = entrance
        , sSourceMap   = [(center, R, ann)]
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
            [ NotEq front p | p <- S.toList (surrounding frontExtension) ]
        }
      where
        -- The extension for a literal
        frontExtension = take int frontForever

        -- Infinite list of pixels that extend to the right
        frontForever = do
            x <- [0 ..]
            y <- take (upH + 1) [0, -1..] ++ take downH [1..]
            pure $ move y D $ move x R front

        entrance = max 1 upH
        int      = fromIntegral n
        left     = move 1 U center
        center   = Pos 0 entrance
        front    = move 1 R center
        right    = move 1 D center

    Import ann attrs img imgExpr | Just "true" <- lookup "recompile" attrs -> Shape
        { sWidth       = Image.width img
        , sHeight      = Image.height img
        , sEntrance    = Image.height img `div` 2
        , sSourceMap   = [(Pos 0 entrance, R, ann)]
        , sConstraints = recompile img imgExpr
        }
      where
        entrance = Image.height img `div` 2

    Import ann _ img _ -> Shape
        { sWidth       = Image.width img
        , sHeight      = Image.height img
        , sEntrance    = Image.height img `div` 2
        , sSourceMap   = [(Pos 0 entrance, R, ann)]
        , sConstraints = do
            y <- [0 .. Image.height img - 1]
            x <- [0 .. Image.width img - 1]
            let col@(JP.PixelRGBA8 _ _ _ alpha) = Image.pixel x y img
            guard $ alpha /= 0
            pure $ LitEq col (Pos x y)
        }
      where
        entrance = Image.height img `div` 2

  where

    surrounding :: [Pos] -> S.Set Pos
    surrounding area = S.fromList
        [n | p <- area, n <- neighbors p, not (n `S.member` S.fromList area)]
