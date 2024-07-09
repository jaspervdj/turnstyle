{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Expr
    ( Expr (..)
    , mapAnn
    , mapErr
    , freeVars
    , allVars
    , normalizeVars
    , checkVars
    , checkCycles
    , checkErrors
    ) where

import           Data.Either.Validation (Validation (..))
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Data.Void              (Void)
import           Turnstyle.Prim

data Expr ann e v
    = App ann (Expr ann e v) (Expr ann e v)
    | Lam ann v (Expr ann e v)
    | Var ann v
    | Prim ann Prim
    | Lit ann Int
    | Err ann e
    deriving (Eq, Foldable, Functor, Show)

mapAnn :: (a -> b) -> Expr a e v -> Expr b e v
mapAnn m (App ann f x) = App  (m ann) (mapAnn m f) (mapAnn m x)
mapAnn m (Lam ann v b) = Lam  (m ann) v (mapAnn m b)
mapAnn m (Var ann v)   = Var  (m ann) v
mapAnn m (Prim ann p)  = Prim (m ann) p
mapAnn m (Lit ann l)   = Lit  (m ann) l
mapAnn m (Err ann err) = Err  (m ann) err

mapErr :: (err0 -> err1) -> Expr ann err0 v -> Expr ann err1 v
mapErr m (App ann f x) = App  ann (mapErr m f) (mapErr m x)
mapErr m (Lam ann v b) = Lam  ann v (mapErr m b)
mapErr _ (Var ann v)   = Var  ann v
mapErr _ (Prim ann p)  = Prim ann p
mapErr _ (Lit ann l)   = Lit  ann l
mapErr m (Err ann err) = Err  ann (m err)

-- | Free variables in an expression.
freeVars :: Ord v => Expr ann e v -> S.Set v
freeVars (App _ f x)    = freeVars f <> freeVars x
freeVars (Lam _ v body) = S.delete v $ freeVars body
freeVars (Var _ v)      = S.singleton v
freeVars (Prim _ _)     = S.empty
freeVars (Lit _ _)      = S.empty
freeVars (Err _ _)      = S.empty

-- | All variables in an expression.
allVars :: Ord v => Expr ann e v -> S.Set v
allVars = foldMap S.singleton

normalizeVars :: forall ann e v. Ord v => Expr ann e v -> Expr ann e Int
normalizeVars = go 0 M.empty
  where
    go :: Int -> M.Map v Int -> Expr ann e v -> (Expr ann e Int)
    go fresh vars (App ann x y) = App ann (go fresh vars x) (go fresh vars y)
    go fresh vars (Lam ann v x) = case M.lookup v vars of
        Just n  -> Lam ann n (go fresh vars x)
        Nothing -> Lam ann fresh (go (fresh + 1) (M.insert v fresh vars) x)
    go fresh vars (Var ann v)   = case M.lookup v vars of
        Nothing -> Var ann fresh
        Just n  -> Var ann n
    go _ _ (Prim ann p)  = Prim ann p
    go _ _ (Lit ann l)   = Lit ann l
    go _ _ (Err ann e)   = Err ann e

-- | Checks that all variables are bound.
checkVars
    :: Ord v
    => (v -> e)      -- ^ Construct error for unbound variable
    -> Expr ann e v  -- ^ Expression to check
    -> Expr ann e v  -- ^ Expression with additional errors
checkVars mkError = go S.empty
  where
    go vars (App ann f x) = App ann (go vars f) (go vars x)
    go vars (Lam ann v b) = Lam ann v $ go (S.insert v vars) b
    go vars (Var ann v)   =
        if S.member v vars then Var ann v else Err ann (mkError v)
    go _    (Prim ann p)  = Prim ann p
    go _    (Lit ann l)   = Lit ann l
    go _    (Err ann err) = Err ann err

-- | Finds cyclic expressions by using comparison on the annotation, assuming
-- this represents some sort of position.
checkCycles
    :: Ord ann
    => (Expr ann e v -> e)  -- ^ Construct cyclic error
    -> Expr ann e v         -- ^ Expression to check
    -> Expr ann e v         -- ^ Expression with additional errors
checkCycles mkError = go S.empty
  where
    go visited expr = case expr of
        App ann f x
            | ann `S.member` visited -> Err ann (mkError expr)
            | otherwise              ->
                let visited' = S.insert ann visited in
                App ann (go visited' f) (go visited' x)
        Lam ann v b
            | ann `S.member` visited -> Err ann (mkError expr)
            | otherwise              -> Lam ann v $ go (S.insert ann visited) b
        Var  _ _ -> expr
        Prim _ _ -> expr
        Lit  _ _ -> expr
        Err  _ _ -> expr

-- | Removes errors from an expression.
checkErrors :: Expr ann e v -> Validation (NonEmpty (ann, e)) (Expr ann Void v)
checkErrors (App ann f x) = App ann <$> checkErrors f <*> checkErrors x
checkErrors (Lam ann v b) = Lam ann v <$> checkErrors b
checkErrors (Var ann v)   = pure $ Var ann v
checkErrors (Prim ann p)  = pure $ Prim ann p
checkErrors (Lit ann l)   = pure $ Lit ann l
checkErrors (Err ann err) = Failure ((ann, err) :| [])
