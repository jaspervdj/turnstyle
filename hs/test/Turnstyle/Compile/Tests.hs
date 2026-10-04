{-# LANGUAGE OverloadedStrings #-}
module Turnstyle.Compile.Tests
    ( tests
    ) where

import qualified Codec.Picture           as JP
import           Control.Monad           (when)
import           Data.Either.Validation  (Validation (..))
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import qualified Data.Set                as S
import qualified Data.Text               as T
import qualified Data.Vector.Unboxed     as VU
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (Assertion, assertBool, assertFailure,
                                          testCase, (@?=))
import qualified Test.Tasty.QuickCheck   as QC
import           Turnstyle.Compile
import           Turnstyle.Compile.Paint (defaultPalette)
import qualified Turnstyle.Eval          as E
import           Turnstyle.Eval          (eval)
import           Turnstyle.Eval.Tests    (MockEvalInput (..),
                                          MockEvalOutput (..),
                                          emptyMockEvalInput, mockEvalIO)
import           Turnstyle.Expr
import           Turnstyle.Expr.Tests
import           Turnstyle.Image
import           Turnstyle.JuicyPixels
import           Turnstyle.Parse
import           Turnstyle.Text          (exprToSugar, parseSugar)
import           Turnstyle.Text.Pretty   (prettyAttributes)
import           Turnstyle.Text.Sugar    (Attributes)

tests :: TestTree
tests = testGroup "Turnstyle.Compile"
    [ QC.testProperty "parse . compile" $ \(GenExpr expr) ->
        let sugar = exprToSugar (show <$> expr) in
        case compile defaultCompileOptions sugar of
            Left err -> error $ "compile error: " ++ show err
            Right cr -> case checkErrors (parseImage Nothing (JuicyPixels (crImage cr))) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed -> toDeBruijn expr == toDeBruijn parsed
    , QC.testProperty "parse . compile (opt)" $ \(GenExpr expr) ->
        let sugar = exprToSugar (show <$> expr) in
        case compile defaultCompileOptions {coOptimize = True, coBudget = 10} sugar of
            Left err -> error $ "compile error: " ++ show err
            Right cr -> case checkErrors (parseImage Nothing (JuicyPixels (crImage cr))) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed -> toDeBruijn expr == toDeBruijn parsed
    , rot13 "rot13" []
    , rot13 "rot13 (recompile)" [("recompile", "true")]
    , testGroup "defaultPalette" $
        [ testCase "length" $
            let len = length defaultPalette in
            assertBool ("insufficient: " ++ show len) (len >= 64)
        , testCase "quality" $ assertBool "colors are not unique" $
            length defaultPalette == S.size (S.fromList defaultPalette)
        ]
    , testCase "layout" $ do
        sugar <- either (fail . show) pure $ parseSugar
            "layout.txt" "\\@layout=\"front\"x. \\@layout=\"front\" x. @layout=\"center\" x"
        cr <- either (fail . show) pure $ compile defaultCompileOptions sugar
        expectPattern (JuicyPixels (crImage cr)) $ T.unlines
            [ "AAB?"
            , "CCCB"
            , "DDD?"
            ]
    ]

rot13 :: String -> Attributes -> TestTree
rot13 name importAttrs = testCase name $ do
    sugar <- either (fail . show) pure $ parseSugar "rot13.txt" src
    cr <- either (fail . show) pure $ compile
        defaultCompileOptions {coImports = M.singleton "y.png" yImage}
        sugar
    let expr = parseImage Nothing (JuicyPixels (crImage cr))
    (evalIO, evalOutputIO) <- mockEvalIO $
        emptyMockEvalInput {mockEvalInChars = "abc\ndef\n"}
    result <- eval evalIO expr
    case result of
        E.Lit 0 -> pure ()
        _       -> assertFailure "expected 0"
    outChars <- mockEvalOutChars <$> evalOutputIO
    outChars @?= reverse "nop\nqrs\n"
  where
    src = unlines
        [ "LET y = IMPORT " ++ prettyAttributes importAttrs ++ " \"y.png\" IN"
        , "LET char_a = num_add (num_mul 10 9) 7 IN"
        , "LET char_z = num_add char_a 25 IN"
        , "LET and = λp q. p q p IN"
        , "LET alpha = λn. and (cmp_gt n (num_sub char_a 1))"
        , "        (cmp_lt n (num_add 1 char_z)) IN"
        , "LET rot13 = λn. (alpha n)"
        , "        (num_add char_a (num_mod (num_add 13 (num_sub n char_a)) 26))"
        , "        n IN"
        , "y (λrec. in_char (λn. out_char (rot13 n) rec) (num_sub 1 1))"
        ]

yImage :: JuicyPixels
yImage = mkJuicyPixels
    [ [a, a, a, a, y, a, a]
    , [a, a, g, y, b, y, a]
    , [a, r, g, b, g, b, y]
    , [y, y, g, b, r, y, a]
    , [r, r, r, r, y, a, a]
    , [y, y, a, b, g, a, a]
    , [y, b, g, b, g, r, a]
    , [a, y, b, y, g, a, a]
    , [a, a, y, a, a, a, a]
    ]
  where
    a = JP.PixelRGBA8   0   0   0   0
    y = JP.PixelRGBA8 255   0   0 255
    g = JP.PixelRGBA8 255 255   0 255
    b = JP.PixelRGBA8   0 255 255 255
    r = JP.PixelRGBA8   0   0 255 255

mkJuicyPixels :: [[Pixel JuicyPixels]] -> JuicyPixels
mkJuicyPixels pixels = JuicyPixels $ JP.generateImage
    (\x y -> pixels !! y !! x) (length (head pixels)) (length pixels)

expectPattern :: JuicyPixels -> T.Text -> Assertion
expectPattern actual expected = do
    asciiImage <- either fail pure $ textToAsciiImage expected
    let actualDimensions = (width actual, height actual)
        expectedDimensions@(w, h) = (width asciiImage, height asciiImage)
    when (actualDimensions /= expectedDimensions) $ assertFailure $
        "expectPattern: expected image dimensions " ++
        show expectedDimensions ++ " but got " ++ show actualDimensions

    let go _ _ _ faults [] | null faults = Right ()
        go _ _ visual _ [] = Left $ AsciiImage w h $ VU.generate (w * h) $ \idx ->
            let (y, x) = idx `divMod` w in
            fromMaybe ' ' $ M.lookup (x, y) visual
        go charToPixel pixelToChar visual faults (pos@(x, y) : ps) =
            let actualPixel = pixel x y actual
                expectedPixel = pixel x y asciiImage in
            case M.lookup expectedPixel charToPixel of
                _ | expectedPixel == '?' ->
                  go charToPixel pixelToChar (M.insert pos '?' visual) faults ps
                Nothing ->
                    go
                        (M.insert expectedPixel actualPixel charToPixel)
                        (M.insert actualPixel expectedPixel pixelToChar)
                        (M.insert pos expectedPixel visual)
                        faults
                        ps
                Just otherPixel -> case M.lookup actualPixel pixelToChar of
                    Just c | c == expectedPixel && actualPixel == otherPixel -> go
                        charToPixel
                        pixelToChar
                        (M.insert pos c visual)
                        faults
                        ps
                    Just c -> go
                        charToPixel
                        pixelToChar
                        (M.insert pos c visual)
                        (pos : faults)
                        ps
                    Nothing -> go
                        charToPixel
                        pixelToChar
                        (M.insert pos '?' visual)
                        (pos : faults)
                        ps

    let faults = go M.empty M.empty M.empty [] [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
    case faults of
        Left visual -> assertFailure $
            "does not match expected pattern: expected:\n\n" ++
            unlines (map ("    " ++ ) (lines $ T.unpack $ asciiImageToText asciiImage)) ++
            "\nbut got:\n\n" ++
            unlines (map ("    " ++ ) (lines $ T.unpack $ asciiImageToText visual))
        Right _ -> pure ()
