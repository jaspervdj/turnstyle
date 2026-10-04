{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Compile.Shake
    ( shakeSoft
    , shakeHard
    , shakeMinimal
    , shakeFirm
    ) where

import           Control.Monad          (mplus)
import           Data.Foldable          (toList)
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.Set               as S
import           System.Random          (RandomGen, uniform, uniformR)
import           Turnstyle.Compile.Expr

shakeOnce
    :: (Expr ann v -> [Expr ann v])
    -> Expr ann v -> [NonEmpty (Expr ann v)]
shakeOnce shakeChild = go id
  where
    go mkExpr expr =
        (case mkExpr <$> shakeChild expr of
            []     -> []
            c : cs -> [c :| cs]) ++
        case expr of
            Import _ _ _ _ -> []
            App ann layout f x ->
                go (\f' -> mkExpr (App ann layout f' x)) f ++
                go (\x' -> mkExpr (App ann layout f x')) x
            Lam ann layout v b ->
                go (\b' -> mkExpr (Lam ann layout v b')) b
            Var _ _ _ -> []
            Prim _ _ -> []
            Lit _ _ _ -> []

shakeExpr :: Expr ann v -> [Expr ann v]
shakeExpr (App ann AppLeftRight  f x) = [App ann l f x | l <- [AppLeftFront, AppFrontRight]]
shakeExpr (App ann AppLeftFront  f x) = [App ann l f x | l <- [AppLeftRight, AppFrontRight]]
shakeExpr (App ann AppFrontRight f x) = [App ann l f x | l <- [AppLeftRight, AppLeftFront]]
shakeExpr (Lam ann LamLeft       v b) = [Lam ann l v b | l <- [LamRight, LamFront]]
shakeExpr (Lam ann LamRight      v b) = [Lam ann l v b | l <- [LamLeft, LamFront]]
shakeExpr (Lam ann LamFront      v b) = [Lam ann l v b | l <- [LamLeft, LamRight]]
shakeExpr (Lit ann (LitLayout u d) l) =
    [Lit ann (LitLayout u' d) l | u' <- [u - 1, u + 1], u' >= 0, u' + d <= fromIntegral l] ++
    [Lit ann (LitLayout u d') l | d' <- [d - 1, d + 1], d' >= 0, u + d' <= fromIntegral l]
shakeExpr _                       = []

shakeSoft :: RandomGen g => Expr ann v -> g -> Maybe (Expr ann v, g)
shakeSoft expr0 gen0 = case shakeOnce shakeExpr expr0 of
    [] -> Nothing
    once ->
        let (onceIdx, gen1) = uniformR (0, length once - 1) gen0
            child = toList $ once !! onceIdx
            (childIdx, gen2) = uniformR (0, length child - 1) gen1 in
        Just (child !! childIdx, gen2)

-- | We randomly generate layouts for programs, but cannot do so in a completely
-- context-free way.  In particular, 'LamFront' and 'VarCenter' are troublesome.
-- By using 'Id' expressions liberally, we don't need to concern ourselves about
-- relative sizes at this stage.  However, consider the following simple layout:
--
-- > Lam LamFront 0 (VarFront 0)
--
-- We cannot draw this program, even with infinite 'Id' expressions.  The reason
-- for that is, if you look at the program as a tree, 'LamFront' and 'VarCenter'
-- introduce constraints on the colors of the paths of that tree.  These are the
-- only cases, all other expressions layouts only ever set constraints on pixels
-- next to that path.
--
-- More formally, you could say that only the specifications for the problematic
-- duo 'LamFront' and 'VarCenter' reference the color of the 'C' pixel.
data ShakeContext v = ShakeContext
    { scPath    :: Maybe v
    , scNotPath :: S.Set v
    } deriving (Show)

instance Ord v => Semigroup (ShakeContext v) where
    l <> r = ShakeContext
        { scPath    = scPath    l `mplus` scPath    r
        , scNotPath = scNotPath l <>      scNotPath r
        }

instance Ord v => Monoid (ShakeContext v) where
    mempty = ShakeContext Nothing S.empty

excludePath :: v -> ShakeContext v
excludePath = ShakeContext Nothing . S.singleton

excludeOwnPath :: Ord v => ShakeContext v -> ShakeContext v
excludeOwnPath = maybe mempty excludePath . scPath

lambdaLayouts :: Ord v => v -> ShakeContext v -> [LamLayout]
lambdaLayouts var (ShakeContext path notPath)
    | Just v <- path, v == var = [LamFront]
    | Just v <- path, v /= var = [LamLeft ,LamRight]
    | var `S.member` notPath = [LamLeft, LamRight]
    | otherwise = [LamLeft , LamRight , LamFront]

lambdaContext :: Ord v => v -> LamLayout -> ShakeContext v -> ShakeContext v
lambdaContext var layout (ShakeContext path notPath) = case layout of
    LamFront -> ShakeContext (Just var) notPath
    LamLeft  -> ShakeContext path (S.insert var notPath)
    LamRight -> ShakeContext path (S.insert var notPath)

data ShakeStrategy
    = ShakeHard
    | ShakeMinimal
    deriving (Eq, Show)

shake
    :: forall ann v g. (Ord v, RandomGen g)
    => ShakeStrategy
    -> Expr ann v -> ShakeContext v -> g
    -> (Expr ann v, ShakeContext v, g)
shake strategy expr ctx g0 = case expr of
    (App ann old f x) ->
        let (layout, g1) = choose old [AppLeftRight, AppLeftFront, AppFrontRight] g0
            -- If there is a scPath, it needs to be added to scNotPath
            -- here, since it must be different?
            --
            -- Consider:
            --
            --     Lam LamLeft 0
            --         (App AppLeftFront
            --             (Lam LamFront 0 (Prim (PNumOp NumOpDivide)))
            --             (Var VarFront 0))
            --
            -- The top-level lambda locks 0 out from the path.  The App
            -- must choose a different color so we forget about that later.
            -- But the two paths started by app must be the same.
            -- This makes varfront and lamfront inconsistent.
            argCtx = excludeOwnPath ctx in
        -- uniformly shake f or x first
        dual
            (\g2 ->
                let (x', ctx1, g3) = shake strategy x argCtx g2
                    (f', ctx2, g4) = shake strategy f ctx1 g3 in
                (App ann layout f' x', ctx <> excludeOwnPath ctx2, g4))
            (\g2 ->
                let (f', ctx1, g3) = shake strategy f argCtx g2
                    (x', ctx2, g4) = shake strategy x ctx1 g3 in
                (App ann layout f' x', ctx <> excludeOwnPath ctx2, g4))
            g1

    (Lam ann old v b) ->
        let layouts = lambdaLayouts v ctx
            (layout, g1) = choose old layouts g0
            ctx1 = lambdaContext v layout ctx
            (b', ctx2, g2) = shake strategy b ctx1 g1 in
        (Lam ann layout v b', ctx <> ctx2, g2)

    (Lit ann old l) | strategy == ShakeMinimal -> (Lit ann old l, ctx, g0)
    (Lit ann _ l) ->
        let (u, g1) = uniformR (0, l) g0
            (d, g2) = uniformR (0, l - u) g1 in
        (Lit ann (LitLayout (fromIntegral u) (fromIntegral d)) l, ctx, g2)

    Var ann old v -> case scPath ctx of
        -- Only one choice.
        Just v' | v' == v -> (Var ann VarCenter v, ctx, g0)
        -- Possibly more choices but minimal shaking.
        _ | VarCenter <- old, ShakeMinimal <- strategy
          , v `S.notMember` scNotPath ctx -> (Var ann VarCenter v, ctx, g0)
        -- Currently we always produce VarFront, but we could introduce a bit of
        -- randomness here as well.
        _ -> (Var ann VarFront v, ctx <> excludePath v, g0)

    Prim   _ _     -> (expr, ctx, g0)
    Import _ _ _ _ -> (expr, ctx, g0)

  where
    choose _old [x] g = (x, g)
    choose old options g | old `elem` options && strategy == ShakeMinimal =
        (old, g)
    choose _old l g =
        let (idx, g') = uniformR (0, length l - 1) g in (l !! idx, g')

    dual :: (g -> b) -> (g -> b) -> g -> b
    dual f g rg0 =
        let (pickF, rg1) = uniform rg0 in
        (if pickF then f else g) rg1

shakeHard
    :: forall ann g v. (Ord v, RandomGen g)
    => Expr ann v -> g -> (Expr ann v, g)
shakeHard expr0 gen0 =
    let (expr1, _, gen1) = shake ShakeHard expr0 mempty gen0 in
    (expr1, gen1)

shakeMinimal
    :: forall ann g v. (Ord v, RandomGen g)
    => Expr ann v -> g -> (Expr ann v, g)
shakeMinimal expr0 gen0 =
    let (expr1, _, gen1) = shake ShakeMinimal expr0 mempty gen0 in
    (expr1, gen1)

-- | This traverses the entire expression tree, and at every node, lazily shakes
-- the entire subtree.  This is then returned as a list, with the intention that
-- the caller can randomly pick an expression with a shaken subtree.
shakeSubtree
    :: forall ann v g. (Ord v, RandomGen g)
    => Expr ann v -> [g -> (Expr ann v, g)]
shakeSubtree topLevelExpr =
    map (\f -> \g0 -> let (e, _, g1) = f g0 in (e, g1)) $
    go (\e c g -> (e, c, g)) topLevelExpr mempty
  where
    go
        :: (Expr ann v -> ShakeContext v -> g -> (Expr ann v, ShakeContext v, g))
        -> Expr ann v
        -> ShakeContext v
        -> [g -> (Expr ann v, ShakeContext v, g)]
    go mkExpr expr ctx =
        [ \gen0 ->
            let (expr1, ctx1, gen1) = shake ShakeHard expr ctx gen0 in
            mkExpr expr1 ctx1 gen1
        ] ++ case expr of
            App ann old f x ->
                go (\f' ctx1 gen1 ->
                        let (x', ctx2, gen2) = shake ShakeMinimal x ctx1 gen1 in
                        mkExpr (App ann old f' x') (ctx <> excludeOwnPath ctx2) gen2)
                    f
                    (excludeOwnPath ctx) ++
                go (\x' ctx1 gen1 ->
                        let (f', ctx2, gen2) = shake ShakeMinimal f ctx1 gen1 in
                        mkExpr (App ann old f' x') (ctx <> excludeOwnPath ctx2) gen2)
                    x
                    (excludeOwnPath ctx)

            Lam ann old v b ->
                go (\b' ctx1 gen1 ->
                       mkExpr (Lam ann old v b') (ctx <> ctx1) gen1)
                   b
                   (lambdaContext v old ctx)

            _ -> []

shakeFirm :: forall ann v g. (Ord v, RandomGen g) => Expr ann v -> g -> (Expr ann v, g)
shakeFirm expr gen0 =
    let options = shakeSubtree expr
        (idx, gen1) = uniformR (0, length options - 1) gen0 in
    (options !! idx) gen1
