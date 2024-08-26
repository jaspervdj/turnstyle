{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Compile.Recompile
    ( recompile
    ) where

import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           Turnstyle.Compile.Constraint
import           Turnstyle.Expr
import           Turnstyle.Image
import           Turnstyle.Parse
import           Turnstyle.Quattern           (Quattern (..))
import           Turnstyle.TwoD

recompile
    :: forall img err. (Image img, Ord (Pixel img))
    => img -> Expr Ann err (Pixel img) -> [ColorConstraint Pos]
recompile img = exprToConstraints img S.empty M.empty

exprToConstraints
    :: forall img err. (Image img, Ord (Pixel img))
    => img
    -> S.Set Ann
    -> M.Map (Pixel img) Pos
    -> Expr Ann err (Pixel img)
    -> [ColorConstraint Pos]
exprToConstraints img visited ctx expr
    | ann `S.member` visited = []
    | otherwise              = constraints ++ children
  where
    ann      = getAnn expr
    visited' = S.insert ann visited
    children = case expr of
        Lam _ _ b -> exprToConstraints img visited' ctx' b
        App _ lhs rhs ->
            exprToConstraints img visited' ctx' lhs ++
            exprToConstraints img visited' ctx' rhs
        Id _ e -> exprToConstraints img visited' ctx' e
        _ -> []

    relPixel rel = let (Pos px py) = relPos pos dir rel in pixel px py img
    (pos, dir, quattern) = ann

    ctx' = case quattern of
        AABC -> M.insert (relPixel RightPos)  r ctx
        ABCB -> M.insert (relPixel LeftPos)   l ctx
        ABBC -> M.insert (relPixel CenterPos) c ctx
        _    -> ctx

    constraints = case quattern of
        AAAA -> [Eq l c, Eq l f, Eq l r]
        AAAB -> [Eq l c, Eq l f, NotEq l r] ++
            [Eq (ctx M.! relPixel RightPos) r]
        AABA -> [Eq l c, NotEq l f, Eq l r] ++
            [Eq (ctx M.! relPixel FrontPos) f]
        AABB -> [Eq l c, NotEq l f, Eq f r]
        AABC -> [Eq l c, NotEq l f, NotEq l r, NotEq f r]
        ABAA -> [NotEq l c, Eq l f, Eq l r] ++
            [Eq (ctx M.! relPixel CenterPos) c]
        ABAB -> [NotEq l c, Eq l f, Eq c r]
        ABAC -> [NotEq l c, Eq l f, NotEq l r, NotEq c r]
        ABBA -> [NotEq l c, Eq c f, Eq l r]
        ABBB -> [NotEq l c, Eq c f, Eq c r] ++
            [Eq (ctx M.! relPixel LeftPos) l]
        ABBC -> [NotEq l c, Eq c f, NotEq l r, NotEq c r]
        ABCA -> [NotEq l c, NotEq l f, NotEq c f, Eq l r]
        ABCB -> [NotEq l c, NotEq l f, NotEq c f, Eq c r]
        ABCC -> [NotEq l c, NotEq l f, NotEq c f, Eq f r]
        ABCD ->
            [NotEq l c, NotEq l f, NotEq c f, NotEq l r, NotEq c r, NotEq f r] ++
            contiguousConstrains l ++
            contiguousConstrains f ++
            contiguousConstrains r

    l = relPos pos dir LeftPos
    c = relPos pos dir CenterPos
    f = relPos pos dir FrontPos
    r = relPos pos dir RightPos

    contiguousConstrains p =
        let inside = contiguous p img
            border = S.fromList $
                filter (not . (`S.member` inside)) $
                concatMap neighbors (S.toList inside) in
        [Eq p i | i <- S.toList inside, i /= p] ++
        [NotEq p b | b <- S.toList border]

