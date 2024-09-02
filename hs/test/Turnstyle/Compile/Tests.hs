module Turnstyle.Compile.Tests
    ( tests
    ) where

import qualified Codec.Picture          as JP
import           Data.Either.Validation (Validation (..))
import qualified Data.Map               as M
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (testCase, (@?=))
import qualified Test.Tasty.QuickCheck  as QC
import           Turnstyle.Compile
import qualified Turnstyle.Eval         as E
import           Turnstyle.Eval         (eval)
import           Turnstyle.Eval.Tests   (EvalState (..), emptyEvalState,
                                         runEvalPure)
import           Turnstyle.Expr
import           Turnstyle.Expr.Tests
import           Turnstyle.Image
import           Turnstyle.JuicyPixels
import           Turnstyle.Parse
import           Turnstyle.Text         (exprToSugar, parseSugar)

tests :: TestTree
tests = testGroup "Turnstyle.Compile"
    [ QC.testProperty "parse . compile" $ \(GenExpr expr) ->
        let sugar = exprToSugar (show <$> expr) in
        case compile defaultCompileOptions sugar of
            Left err -> error $ "compile error: " ++ show err
            Right img -> case checkErrors (parseImage Nothing (JuicyPixels img)) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed -> toDeBruijn expr == toDeBruijn parsed
    , QC.testProperty "parse . compile (opt)" $ \(GenExpr expr) ->
        let sugar = exprToSugar (show <$> expr) in
        case compile defaultCompileOptions {coOptimize = True, coBudget = 10} sugar of
            Left err -> error $ "compile error: " ++ show err
            Right img -> case checkErrors (parseImage Nothing (JuicyPixels img)) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed -> toDeBruijn expr == toDeBruijn parsed
    , testCase "rot13" $ do
        sugar <- either (fail . show) pure $ parseSugar "rot13.txt" rot13
        img <- either (fail . show) pure $ compile
            defaultCompileOptions {coImports = M.singleton "y.png" yImage}
            sugar
        let expr = parseImage Nothing (JuicyPixels img)
            (result, finalState) = runEvalPure (eval expr)
                emptyEvalState {esInChars = "abc\ndef\n"}
        result @?= Right (E.Lit 0)
        esOutChars finalState @?= reverse "nop\nqrs\n"
    ]
  where
    -- TODO: test this with and without recoloring
    rot13 = unlines
        [ "LET y = IMPORT \"y.png\" IN"
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
