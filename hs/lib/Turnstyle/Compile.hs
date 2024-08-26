module Turnstyle.Compile
    ( CompileOptions (..)
    , defaultCompileOptions

    , CompileError (..)
    , SolveError (..)

    , compile
    ) where

import qualified Codec.Picture                        as JP
import           Data.Bifunctor                       (first)
import           Data.Either.Validation               (Validation (..))
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.Map                             as M
import           Data.Ord                             (Down (..))
import           Data.Void                            (Void)
import           System.Random                        (mkStdGen)
import           Turnstyle.Compile.Bound
import           Turnstyle.Compile.Expr
import           Turnstyle.Compile.Paint
import           Turnstyle.Compile.Shake
import           Turnstyle.Compile.Shape
import qualified Turnstyle.Compile.SimulatedAnnealing as SA
import           Turnstyle.Compile.Solve
import qualified Turnstyle.Expr                       as E
import           Turnstyle.JuicyPixels                (JuicyPixels)
import           Turnstyle.Parse                      (Ann, ParseError,
                                                       parseImage)
import qualified Turnstyle.Text.Sugar                 as S
import           Turnstyle.TwoD

data CompileOptions = CompileOptions
    { coImports   :: M.Map FilePath JuicyPixels
    , coOptimize  :: Bool
    , coSeed      :: Int
    , coBudget    :: Int
    , coHillClimb :: Bool
    , coRestarts  :: Int
    }

defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions M.empty False 12345 1000 False 5

data CompileError ann
    = UnboundVars (NonEmpty (ann, String))
    | UnknownImport ann FilePath
    | BadImport ann FilePath (NonEmpty (Ann, ParseError))
    | SolveError (SolveError Pos)
    deriving (Show)

compile
    :: CompileOptions -> S.Sugar Void ann
    -> Either (CompileError ann) (JP.Image JP.PixelRGBA8)
compile _ expr | err : errs <- checkVars expr = do
    Left $ UnboundVars (err :| errs)
compile opts expr = do
    expr0 <- fromSugar (\ann path -> case M.lookup path (coImports opts) of
        Nothing -> Left $ UnknownImport ann path
        Just jp -> case E.checkErrors (parseImage Nothing jp) of
            Success e   -> pure $ Import jp e
            Failure err -> Left $ BadImport ann path err) expr

    let neighbour l g = case shake l g of
             Just (l', g')
                 | Right _ <- solve $ sConstraints (exprToShape l') ->
                     (l', g')
             _ -> (l, g)

        expr1
            | not (coOptimize opts) = expr0
            | otherwise = fst $ withRestarts
                (coRestarts opts)
                (Down . scoreLayout)
                (case coHillClimb opts of
                    True -> hillWalk
                        (coBudget opts)
                        (Down . scoreLayout)
                        neighbour
                    False -> SA.run
                        SA.defaultOptions
                            { SA.oGiveUp    = Just (coBudget opts)
                            , SA.oScore     = fromIntegral . negate . scoreLayout
                            , SA.oNeighbour = neighbour
                            })
                expr0 (mkStdGen (coSeed opts))
        shape = exprToShape expr1
    colors <- first SolveError (solve $ sConstraints shape)
    pure $ paint defaultPalette colors shape

scoreLayout :: Ord v => Expr v -> Int
scoreLayout expr =
    let shape = exprToShape expr in
    -- Minimize area
    2 * (sWidth shape + sHeight shape) +
    -- Try to get a square
    abs (sWidth shape - sHeight shape) +
    -- Align entrance near center
    4 * abs (sHeight shape `div` 2 - sEntrance shape)

withRestarts
    :: Ord n
    => Int
    -> (a -> n)
    -> (a -> g -> (a, g))
    -> a -> g -> (a, g)
withRestarts n score f x0 g0 =
    let (x1, g1) = f x0 g0 in
    go 0 x1 (score x1) g1
  where
    go i best bestScore gen0
        | i >= n                 = (best, gen0)
        | nextScore >= bestScore = go (i + 1) next nextScore gen1
        | otherwise              = go (i + 1) best bestScore gen1
      where
        (next, gen1) = f x0 gen0
        nextScore    = score next

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
