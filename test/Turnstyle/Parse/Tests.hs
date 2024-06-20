module Turnstyle.Parse.Tests
    ( tests
    ) where

import           Data.Either.Validation (Validation (..))
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (testCase, (@?=))
import           Turnstyle.Expr
import           Turnstyle.JuicyPixels  (loadImage)
import           Turnstyle.Parse        (parseImage)
import           Turnstyle.Prim
import           Turnstyle.Scale        (autoScale)

tests :: TestTree
tests = testGroup "Turnstyle.Parse"
    [ example "examples/minimal-large.png" $
        num_sub (lit 1) (lit 1)

    , example "examples/pi-large.png" $ out_num
        (num_div
            (lit 749)
            (app
                (lam 0 (app
                    (lam 1 (num_mul (var 1) (var 1)))
                    [num_div (var 0) (lit 2)]))
                [lit 31]))
        (num_sub (lit 1) (lit 1))

    , example "examples/y-large.png" yc

    , example "examples/turnstyle-large.png" $ app
        yc
        [ lam 0 $ out_char
            (num_add (lit 34) (num_mul (lit 94) (lit 94)))
            (var 0)
        ]
    ]
  where
    example path expected = testCase path $ do
        img <- autoScale <$> loadImage path
        case checkErrors (parseImage Nothing img) of
            Failure errs -> fail $ show errs
            Success expr -> mapAnn (const ()) (normalizeVars expr) @?= expected

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
