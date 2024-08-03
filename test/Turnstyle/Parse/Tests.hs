module Turnstyle.Parse.Tests
    ( tests
    ) where

import           Data.Either.Validation  (Validation (..))
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Void               (Void)
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase, (@?=))
import           Turnstyle.Expr
import           Turnstyle.Expr.Tests
import           Turnstyle.JuicyPixels   (loadImage)
import           Turnstyle.Parse         (parseImage)
import           Turnstyle.Prim
import           Turnstyle.Scale         (autoScale)

data Error = ParseError | CycleError

tests :: TestTree
tests = testGroup "Turnstyle.Parse"
    [ example "examples/minimal.png" $
        num_sub (lit 8) (lit 8)

    , example "examples/pi.png" $ out_num
        (num_div
            (lit 749)
            (app
                (lam 0 (app
                    (lam 1 (num_mul (var 1) (var 1)))
                    [num_div (var 0) (lit 2)]))
                [lit 31]))
        (num_sub (lit 1) (lit 1))

    , example "examples/y.png" yc

    , testCase "examples/loop.png" $ do
        img <- autoScale <$> loadImage "examples/loop.png"
        let parsed = mapErr (const ParseError) (parseImage Nothing img)
        case checkErrors (checkCycles (const CycleError) parsed) of
            Failure ((_, CycleError) :| []) -> pure ()
            _                               -> error "expected a CycleError"
    ]
  where
    example :: FilePath -> Expr () Void Int -> TestTree
    example path expected = testCase path $ do
        img <- autoScale <$> loadImage path
        case checkErrors (parseImage Nothing img) of
            Failure errs -> fail $ show errs
            Success expr -> toDeBruijn expr @?= toDeBruijn expected

    app f xs = foldl (App ()) f xs
    lam      = Lam ()
    var      = Var ()
    prim     = Prim ()
    lit      = Lit ()

    out_num  x k  = app (prim $ POut OutNumber)  [x, k]
    out_char x k  = app (prim $ POut OutChar) [x, k]

    num_add x y = app (prim $ PNumOp NumOpAdd)      [x, y]
    num_sub x y = app (prim $ PNumOp NumOpSubtract) [x, y]
    num_mul x y = app (prim $ PNumOp NumOpMultiply) [x, y]
    num_div x y = app (prim $ PNumOp NumOpDivide)   [x, y]

    yc = lam 0 $ app
        (lam 1 (app (var 0) [app (var 1) [var 1]]))
        [lam 1 (app (var 0) [app (var 1) [var 1]])]
