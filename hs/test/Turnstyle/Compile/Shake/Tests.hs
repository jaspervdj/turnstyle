{-# LANGUAGE OverloadedStrings #-}
module Turnstyle.Compile.Shake.Tests
    ( tests
    ) where

import           System.Random           (mkStdGen)
import           Test.Tasty              (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck   as QC
import qualified Turnstyle.Compile.Expr  as Compile
import           Turnstyle.Compile.Paint (defaultPalette)
import           Turnstyle.Compile.Shake
import Turnstyle.TwoD (Pos)
import           Turnstyle.Compile.Shape
import           Turnstyle.Compile.Solve
import           Turnstyle.Expr.Tests
import           Turnstyle.Expr (normalizeVars)
import qualified Codec.Picture                        as JP

tests :: TestTree
tests = testGroup "Turnstyle.Compile.Shake"
    [ QC.testProperty "shakeHard produces valid layout" $ \(GenExpr expr0) seed ->
       let gen = mkStdGen seed
           expr1 = Compile.fromExpr expr0
           (expr2, _) = shakeHard expr1 gen in
       canBeSolved expr2 $ \_ ->
           Compile.toExpr expr1 QC.=== Compile.toExpr expr2

    , QC.testProperty "shakeMinimal should not modify layout if not needed" $
        \(GenExpr expr) seed ->
            let gen0 = mkStdGen seed
                (shakenHard, gen1) = shakeHard (Compile.fromExpr expr) gen0
                (shakenMinimal, _) = shakeMinimal shakenHard gen1 in
            shakenHard QC.=== shakenMinimal

    , QC.testProperty "shakeFirm produces valid layout" $ \(GenExpr expr0) seed ->
       let gen0 = mkStdGen seed
           (expr1, gen1) = shakeHard (Compile.fromExpr expr0) gen0
           (expr2, _) = shakeFirm expr1 gen1 in
       canBeSolved expr2 $ \_ ->
           Compile.toExpr expr1 QC.=== Compile.toExpr expr2
    ]

canBeSolved
    :: (Show ann, Show v, Ord v)
    => Compile.Expr ann v
    -> ((Pos -> Maybe JP.PixelRGBA8) -> QC.Property)
    -> QC.Property
canBeSolved expr k =
    let shape = exprToShape expr in
    case solve defaultPalette (sConstraints shape) of
        Right solution -> k solution
        Left err -> QC.counterexample (msg err) False
  where
    msg err = "could not solve shaken expr: " ++ show expr ++
        ", solve error: " ++ show err
