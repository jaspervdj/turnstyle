module Turnstyle.Compile
    ( CompileOptions (..)
    , defaultCompileOptions

    , CompileError (..)
    , CompileResult (..)
    , SolveError (..)

    , compile
    ) where

import qualified Codec.Picture                        as JP
import           Data.Bifunctor                       (first)
import           Data.Either.Validation               (Validation (..))
import           Data.Foldable                        (toList)
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.Map                             as M
import           Data.Ord                             (Down (..))
import qualified Data.Set                             as S
import           Data.Void                            (Void)
import           System.Random                        (mkStdGen)
import           Turnstyle.Compile.Bound
import qualified Turnstyle.Compile.Contaminate        as Contaminate
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
import qualified Turnstyle.Text.Sugar                 as Sugar
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
defaultCompileOptions = CompileOptions M.empty False 12345 100 True 1

data CompileError ann
    = UnboundVars (NonEmpty (ann, String))
    | UnknownImport ann FilePath
    | BadImport ann FilePath (NonEmpty (Ann, ParseError))
    | SolveError (Expr ann String) (SolveError Pos)
    deriving (Show)

data CompileResult ann = CompileResult
    { crImage     :: JP.Image JP.PixelRGBA8
    , crSourceMap :: [(Pos, Dir, ann)]
    }

compile
    :: CompileOptions -> Sugar.Sugar Void ann
    -> Either (CompileError ann) (CompileResult ann)
compile _ expr | err : errs <- checkVars expr = do
    Left $ UnboundVars (err :| errs)
compile opts expr = do
    expr0 <- fromSugar (\ann attrs path -> case M.lookup path (coImports opts) of
        Nothing -> Left $ UnknownImport ann path
        Just jp -> case E.checkErrors (parseImage Nothing jp) of
            Success e   -> pure $ Import ann attrs jp e
            Failure err -> Left $ BadImport ann path err) expr

    let palette =
            let contaminated = Contaminate.palette expr0 in
            toList contaminated ++
            filter (not . (`elem` contaminated)) defaultPalette

        neighbour l g = case Just (shakeFirm l g) of
             Just (l', g')
                 | Right _ <- solve palette $ sConstraints (exprToShape l') ->
                     (l', g')
             _ -> (l, g)

        expr1
            | not (coOptimize opts) = expr0
            | otherwise = fst $ withRestarts
                (coRestarts opts)
                (Down . scoreLayout)
                (\initialExpr gen0 ->
                    let (randomizedExpr, gen1) = shakeHard initialExpr gen0 in
                    case coHillClimb opts of
                        True -> hillWalk
                            (coBudget opts)
                            (Down . scoreLayout)
                            neighbour
                            randomizedExpr
                            gen1
                        False -> SA.run
                            SA.defaultOptions
                                { SA.oGiveUp    = Just (coBudget opts)
                                , SA.oScore     = fromIntegral . negate . scoreLayout
                                , SA.oNeighbour = neighbour
                                }
                            randomizedExpr
                            gen1)
                expr0 (mkStdGen (coSeed opts))
        shape = exprToShape expr1
    colors <- first (SolveError expr1) (solve palette $ sConstraints shape)
    pure $ CompileResult
        { crImage     = paint colors shape
        , crSourceMap = sSourceMap shape
        }

scoreLayout :: Ord v => Expr ann v -> Int
scoreLayout expr =
    let shape = exprToShape expr in
    -- Minimize area
    2 * (sWidth shape + sHeight shape) +
    -- Try to get a square
    abs (sWidth shape - sHeight shape) +
    -- Align entrance near center
    4 * abs (sHeight shape `div` 2 - sEntrance shape) +
    -- Minimize empty pixels.  Approximate empty pixels by subtracting mentioned
    -- pixels in the constraints from the size.
    (
        (sWidth shape * sHeight shape) -
        S.size (foldMap (S.fromList . toList) $ sConstraints shape)
    )

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
