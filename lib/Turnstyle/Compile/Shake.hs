module Turnstyle.Compile.Shake
    ( shake
    ) where

import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Foldable (toList)
import           System.Random           (RandomGen, uniformR)
import           Turnstyle.Compile.Shape
import           Turnstyle.Expr

shakeOnce
    :: (Expr ann e v -> [Expr ann e v])
    -> Expr ann e v -> [NonEmpty (Expr ann e v)]
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
            Var _ _ -> []
            Prim _ _ -> []
            Lit _ _ -> []
            Id _ _ -> []
            Err _ _ -> []

shakeExpr :: Expr Layout e v -> [Expr Layout e v]
shakeExpr (App (AppLayout AppLeftRight)  f x) = [App (AppLayout l) f x | l <- [AppLeftFront, AppFrontRight]]
shakeExpr (App (AppLayout AppLeftFront)  f x) = [App (AppLayout l) f x | l <- [AppLeftRight, AppFrontRight]]
shakeExpr (App (AppLayout AppFrontRight) f x) = [App (AppLayout l) f x | l <- [AppLeftRight, AppLeftFront]]
shakeExpr (Lam (LamLayout LamLeft)       v b) = [Lam (LamLayout l) v b | l <- [LamRight, LamStraight]]
shakeExpr (Lam (LamLayout LamRight)      v b) = [Lam (LamLayout l) v b | l <- [LamLeft, LamStraight]]
shakeExpr (Lam (LamLayout LamStraight)   v b) = [Lam (LamLayout l) v b | l <- [LamLeft, LamRight]]
shakeExpr _                                   = []

shake :: RandomGen g => Expr Layout e v -> g -> Maybe (Expr Layout e v, g)
shake expr0 gen0 = case shakeOnce shakeExpr expr0 of
    [] -> Nothing
    once ->
        let (onceIdx, gen1) = uniformR (0, length once - 1) gen0
            child = toList $ once !! onceIdx
            (childIdx, gen2) = uniformR (0, length child - 1) gen1 in
        Just (child !! childIdx, gen2)
