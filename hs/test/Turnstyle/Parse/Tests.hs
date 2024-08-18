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
        num_sub (lit 8) (lit 8 :: Expr () Void Int)

    , example "examples/pi.png" $ out_num
        (num_div
            (lit 1321 :: Expr () Void Int)
            (app
                (lam 0 (app
                    (lam 1 (num_mul (var 1) (var 1)))
                    [num_div (var 0) (lit 2)]))
                [lit 41]))
        (num_sub (lit 1) (lit 1))

    , example "examples/rev.png" $ app
        ((lam "newline")
            (app
                (yc)
                [ lam "rec" $ lam "acc" $ in_char
                    (lam "c" $ cmp_eq
                        (var "c")
                        (var "newline")
                        (app (var "acc")
                            [out_char (var "newline") (app (var "rec") [lam "x" (var "x")])])
                        (app (var "rec")
                            [lam "final" (out_char (var "c") (app (var "acc") [var "final"]))]))
                    (num_sub (var "newline") (var "newline"))
                , lam "x" (var "x")
                ]))
        [lit 10]

    , testCase "examples/loop.png" $ do
        img <- autoScale <$> loadImage "examples/loop.png"
        let parsed = mapErr (const ParseError) (parseImage Nothing img)
        case checkErrors (checkCycles (const CycleError) parsed) of
            Failure ((_, CycleError) :| []) -> pure ()
            _                               -> error "expected a CycleError"
    ]
  where
    example :: Ord v => FilePath -> Expr () Void v -> TestTree
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

    in_char  k l = app (prim $ PIn InChar) [k, l]
    out_num  x k = app (prim $ POut OutNumber)  [x, k]
    out_char x k = app (prim $ POut OutChar) [x, k]

    num_sub x y = app (prim $ PNumOp NumOpSubtract) [x, y]
    num_mul x y = app (prim $ PNumOp NumOpMultiply) [x, y]
    num_div x y = app (prim $ PNumOp NumOpDivide)   [x, y]

    cmp_eq x y t f = app (prim $ PCompare CmpEq) [x, y, t, f]

    yc = lam "f" $ app
        (lam "x" (app (var "f") [app (var "x") [var "x"]]))
        [lam "x" (app (var "f") [app (var "x") [var "x"]])]
