{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Compile.Paint
    ( CompileError (..)
    , SolveError (..)

    , paint
    ) where

import qualified Codec.Picture           as JP
import           Control.Monad           (foldM, when)
import           Data.Bifunctor          (first)
import           Data.Foldable           (toList)
import qualified Data.Graph              as G
import           Data.List               (foldl', transpose)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe, maybeToList)
import qualified Data.Set                as S
import           Turnstyle.Compile.Shape
import           Turnstyle.TwoD

data CompileError
    = SolveError (SolveError Pos)
    deriving (Show)

paint :: Shape -> Either CompileError (JP.Image JP.PixelRGB8)
paint s = do
    colors <- fmap (\(Color i) -> palette !! i) <$>
        first SolveError (solve $ sConstraints s)
    pure $ JP.generateImage
        (\x0 y0 -> fromMaybe background $
            let x1 = x0 - offsetX
                y1 = y0 - offsetY in
            if x1 >= 0 && x1 < sWidth s && y1 >= 0 && y1 < sHeight s
                then M.lookup (Pos x1 y1) colors
                else Nothing)
        (sWidth s)
        (spacingHeight * 2 + 1)
  where
    topHeight     = sEntrance s
    bottomHeight  = sHeight s - sEntrance s - 1
    spacingHeight = max topHeight bottomHeight
    offsetX       = 0
    offsetY       = spacingHeight - sEntrance s
    background    = JP.PixelRGB8 255 255 255

palette :: [JP.PixelRGB8]
palette = concat $ transpose
    [ [JP.PixelRGB8 c 0 0 | c <- steps]
    , [JP.PixelRGB8 0 c 0 | c <- steps]
    , [JP.PixelRGB8 0 0 c | c <- steps]
    , [JP.PixelRGB8 c c 0 | c <- steps]
    , [JP.PixelRGB8 0 c c | c <- steps]
    , [JP.PixelRGB8 c 0 c | c <- steps]
    ]
  where
    steps = reverse $ [63, 127 .. 255]

newtype Component p = Component Int deriving (Eq, Ord)
newtype Color = Color Int deriving (Eq)

data SolveError p
    = Inconsistent p p
    | UnknownVertex p deriving (Show)

solve
    :: forall p. Ord p
    => [ColorConstraint p] -> Either (SolveError p) (M.Map p Color)
solve constraints = do
    ineqGraph <- mkInequalityGraph
    let colors = greedyColor ineqGraph
    pure $ M.mapMaybe (\c -> M.lookup c colors) vertexToComponent
  where
    vertices :: S.Set p
    vertices = foldMap S.singleton (constraints >>= toList)

    equalityGraph :: M.Map p (S.Set p)
    equalityGraph = foldl'
        (\acc constraint -> case constraint of
            NotEq _ _ -> acc
            Eq p q    -> M.insertWith S.union p (S.singleton q) $
                M.insertWith S.union q (S.singleton p) $ acc)
        (M.fromSet (const S.empty) vertices)
        constraints

    components :: [(Component p, G.SCC p)]
    components = zip [Component i | i <- [0 ..]] $ G.stronglyConnComp
        [(p, p, S.toList qs) | (p, qs) <- M.toList equalityGraph]

    vertexToComponent :: M.Map p (Component p)
    vertexToComponent = M.fromList
        [(p, c) | (c, gc) <- components, p <- toList gc]

    mkInequalityGraph
        :: Either (SolveError p) (M.Map (Component p) (S.Set (Component p)))
    mkInequalityGraph = foldM
        (\acc constraint -> case constraint of
            Eq _ _ -> pure acc
            NotEq p q -> do
                pc <- maybe (Left $ UnknownVertex p) pure $ M.lookup p vertexToComponent
                qc <- maybe (Left $ UnknownVertex q) pure $ M.lookup q vertexToComponent
                when (pc == qc) $ Left $ Inconsistent p q
                pure $ M.insertWith S.union pc (S.singleton qc) $
                    M.insertWith S.union qc (S.singleton pc) $ acc)
        M.empty
        constraints

    greedyColor
        :: M.Map (Component p) (S.Set (Component p))
        -> M.Map (Component p) Color
    greedyColor inequalityGraph = foldl'
        (\acc c ->
            let unavailable = do
                    nc <- maybe [] S.toList $ M.lookup c inequalityGraph
                    maybeToList $ M.lookup nc acc
                color = head $ filter
                    (not . (`elem` unavailable))
                    [Color i | i <- [0 ..]] in
            M.insert c color acc)
        M.empty
        (fst <$> components)
