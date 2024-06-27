module Turnstyle.Compile
    ( CompileOptions (..)
    , defaultCompileOptions

    , CompileError (..)
    , SolveError (..)

    , compile
    ) where

import qualified Codec.Picture           as JP
import           Data.Bifunctor          (first)
import           Data.Either.Validation  (Validation (..))
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Ord                (Down (..))
import           Data.Void               (Void)
import           System.Random           (mkStdGen)
import qualified Turnstyle.Compile.SimulatedAnnealing as SA
import           Turnstyle.Compile.Paint
import           Turnstyle.Compile.Shake
import           Turnstyle.Compile.Shape
import           Turnstyle.Compile.Solve
import           Turnstyle.Expr
import           Turnstyle.TwoD

data CompileOptions = CompileOptions
    { coOptimize  :: Bool
    , coSeed      :: Int
    , coBudget    :: Int
    , coHillClimb :: Bool
    }

defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions False 12345 1000 False

data CompileError ann v
    = UnboundVars (NonEmpty (ann, v))
    | SolveError (SolveError Pos)
    deriving (Show)

compile
    :: Ord v
    => CompileOptions -> Expr ann Void v
    -> Either (CompileError ann v) (JP.Image JP.PixelRGB8)
compile _ expr | Failure err <- checkErrors (checkVars expr) = do
    Left $ UnboundVars err
compile opts expr = do
    let expr0 = defaultLayout expr

        neighbour l g = case shake l g of
             Just (l', g')
                 | Right _ <- solve $ sConstraints (exprToShape l') ->
                     (l', g')
             _ -> (l, g)

        expr1
            | not (coOptimize opts) = expr0
            | coHillClimb opts = fst $ hillWalk
                (coBudget opts)
                (Down . scoreLayout)
                neighbour
                expr0
                (mkStdGen (coSeed opts))
            | otherwise = fst $ SA.run
                SA.defaultOptions
                    { SA.oGiveUp    = Just (coBudget opts)
                    , SA.oScore     = fromIntegral . negate . scoreLayout
                    , SA.oNeighbour = neighbour
                    }
                expr0
                (mkStdGen (coSeed opts))
        shape = exprToShape expr1
    colors <- first SolveError (solve $ sConstraints shape)
    pure $ paint colors shape

scoreLayout :: Ord v => Expr Layout Void v -> Int
scoreLayout expr =
    let shape = exprToShape expr in
    -- Minimize area
    (sWidth shape * sHeight shape `div` 2) +
    -- Try to get a square
    abs (sWidth shape - sHeight shape) +
    -- Align entrance near center
    4 * abs (sHeight shape `div` 2 - sEntrance shape)

hillWalk
    :: Ord n
    => Int
    -> (a -> n)
    -> (a -> g -> (a, g))
    -> a -> g -> (a, g)
hillWalk maxSteps score step start = go 0 start (score start) start
  where
    go steps best bestScore current gen0
        | steps >= maxSteps      = (best, gen0)
        | nextScore >= bestScore = go (steps + 1) next nextScore next gen1
        | otherwise              = go (steps + 1) best bestScore best gen1
      where
        (next, gen1) = step current gen0
        nextScore    = score next
