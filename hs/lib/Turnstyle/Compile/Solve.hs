{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Compile.Solve
    ( SolveError (..)
    , solve
    ) where

import           Control.Monad           (foldM, when)
import           Data.Foldable           (toList)
import qualified Data.Graph              as G
import           Data.List               (foldl')
import qualified Data.Map                as M
import           Data.Maybe              (maybeToList)
import qualified Data.Set                as S
import           Turnstyle.Compile.Shape

type Graph v = M.Map v (S.Set v)

addEdge :: Ord v => v -> v -> Graph v -> Graph v
addEdge p q =
    M.insertWith S.union p (S.singleton q) .
    M.insertWith S.union q (S.singleton p)

newtype Component p = Component Int deriving (Eq, Ord)

data SolveError p
    = Inconsistent p p
    | UnknownVertex p
    | NotEnoughColors deriving (Show)

solve
    :: forall p c. (Ord p, Eq c)
    => [c]
    -> [ColorConstraint c p]
    -> Either (SolveError p) (p -> Maybe c)
solve palette constraints = do
    ineqGraph <- mkInequalityGraph
    colors <- greedyColor ineqGraph
    pure $ \p -> M.lookup p vertexToComponent >>= (`M.lookup` colors)
  where
    vertices :: S.Set p
    vertices = foldMap S.singleton (constraints >>= toList)

    equalities, inequalities :: [(p, p)]
    equalities   = [(p, q) | Eq p q <- constraints]
    inequalities = [(p, q) | NotEq p q <- constraints]

    equalityGraph :: Graph p
    equalityGraph = foldl'
        (\acc (p, q) -> addEdge p q acc)
        (M.fromSet (const S.empty) vertices)
        equalities

    components :: [(Component p, G.SCC p)]
    components = zip [Component i | i <- [0 ..]] $ G.stronglyConnComp
        [(p, p, S.toList qs) | (p, qs) <- M.toList equalityGraph]

    vertexToComponent :: M.Map p (Component p)
    vertexToComponent = M.fromList
        [(p, c) | (c, gc) <- components, p <- toList gc]

    mkInequalityGraph :: Either (SolveError p) (Graph (Component p))
    mkInequalityGraph = foldM
        (\acc (p, q) -> do
            pc <- maybe (Left $ UnknownVertex p) pure $ M.lookup p vertexToComponent
            qc <- maybe (Left $ UnknownVertex q) pure $ M.lookup q vertexToComponent
            when (pc == qc) $ Left $ Inconsistent p q
            pure $ addEdge pc qc acc)
        M.empty
        inequalities

    initialColors :: M.Map (Component p) c
    initialColors = M.fromList $ do
        LitEq col p <- constraints
        comp <- maybeToList $ M.lookup p vertexToComponent
        pure (comp, col)

    greedyColor
        :: M.Map (Component p) (S.Set (Component p))
        -> Either (SolveError p) (M.Map (Component p) c)
    greedyColor inequalityGraph = foldM
        (\acc c -> do
            let unavailable = do
                    nc <- maybe [] S.toList $ M.lookup c inequalityGraph
                    maybeToList $ M.lookup nc acc
            color <- case filter (not . (`elem` unavailable)) palette of
                []      -> Left NotEnoughColors
                col : _ -> pure col
            pure $ M.insert c color acc)
        initialColors
        (filter (not . (`M.member` initialColors)) $ fst <$> components)
