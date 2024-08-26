module Turnstyle.Compile.Shake
    ( shake
    ) where

import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Foldable (toList)
import           System.Random           (RandomGen, uniformR)
import           Turnstyle.Compile.Expr

shakeOnce
    :: (Expr v -> [Expr v])
    -> Expr v -> [NonEmpty (Expr v)]
shakeOnce shakeChild = go id
  where
    go mkExpr expr =
        (case mkExpr <$> shakeChild expr of
            []     -> []
            c : cs -> [c :| cs]) ++
        case expr of
            App ann f x ->
                go (\f' -> mkExpr (App ann f' x)) f ++
                go (\x' -> mkExpr (App ann f x')) x
            Lam ann v b ->
                go (\b' -> mkExpr (Lam ann v b')) b
            Var _ -> []
            Prim _ -> []
            Lit _ -> []

shakeExpr :: Expr v -> [Expr v]
shakeExpr (App AppLeftRight  f x) = [App l f x | l <- [AppLeftFront, AppFrontRight]]
shakeExpr (App AppLeftFront  f x) = [App l f x | l <- [AppLeftRight, AppFrontRight]]
shakeExpr (App AppFrontRight f x) = [App l f x | l <- [AppLeftRight, AppLeftFront]]
shakeExpr (Lam LamLeft       v b) = [Lam l v b | l <- [LamRight, LamStraight]]
shakeExpr (Lam LamRight      v b) = [Lam l v b | l <- [LamLeft, LamStraight]]
shakeExpr (Lam LamStraight   v b) = [Lam l v b | l <- [LamLeft, LamRight]]
shakeExpr _                       = []

shake :: RandomGen g => Expr v -> g -> Maybe (Expr v, g)
shake expr0 gen0 = case shakeOnce shakeExpr expr0 of
    [] -> Nothing
    once ->
        let (onceIdx, gen1) = uniformR (0, length once - 1) gen0
            child = toList $ once !! onceIdx
            (childIdx, gen2) = uniformR (0, length child - 1) gen1 in
        Just (child !! childIdx, gen2)
