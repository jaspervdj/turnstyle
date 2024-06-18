module Turnstyle.Parse.Tests
    ( tests
    ) where

import           Data.Either.Validation (Validation (..))
import           Prelude                hiding (subtract)
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
        subtract (lit 5) (lit 5)

    , example "examples/pi-large.png" $ app (prim $ POut OutNumber)
        [ divide
            (lit 1020)
            (app
                (lam 0 (app
                    (lam 1 (multiply (var 1) (var 1)))
                    [divide (var 0) (lit 2)]))
                [lit 36])
        , subtract (lit 1) (lit 1)
        ]

    , example "examples/y-large.png" yc

    , example "examples/turnstyle-large.png" $ app
        yc
        [ lam 0 $ app (prim $ POut OutChar)
            [ add (lit 34) (multiply (lit 94) (lit 94))
            , var 0
            ]
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

    add      x y = app (prim $ PNumOp NumOpAdd)      [x, y]
    subtract x y = app (prim $ PNumOp NumOpSubtract) [x, y]
    multiply x y = app (prim $ PNumOp NumOpMultiply) [x, y]
    divide   x y = app (prim $ PNumOp NumOpDivide)   [x, y]

    yc = lam 0 $ app
        (lam 1 (app (var 0) [app (var 1) [var 1]]))
        [lam 1 (app (var 0) [app (var 1) [var 1]])]
