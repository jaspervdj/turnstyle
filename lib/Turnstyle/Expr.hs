{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Expr
    ( Expr (..)
    , freeVars
    , allVars
    , normalizeVars
    , checkErrors
    ) where

import           Control.Monad.State    (State, evalState, state)
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
normalizeVars expr = evalState (go expr) (M.empty, 0)
  where
    go :: Expr ann e v -> State (M.Map v Int, Int) (Expr ann e Int)
    go (App ann x y) = App ann <$> go x <*> go y
    go (Lam ann v x) = Lam ann <$> var v <*> go x
    go (Var ann v)   = Var ann <$> var v
    go (Prim ann p)  = pure $ Prim ann p
    go (Lit ann l)   = pure $ Lit ann l
    go (Err ann e)   = pure $ Err ann e

    var :: v -> State (M.Map v Int, Int) Int
    var v = state $ \(vars, fresh) -> case M.lookup v vars of
        Just n  -> (n, (vars, fresh))
        Nothing -> (fresh, (M.insert v fresh vars, fresh + 1))

-- | Removes errors from an expression.
checkErrors :: Expr ann e v -> Validation (NonEmpty (ann, e)) (Expr ann Void v)
checkErrors (App ann f x) = App ann <$> checkErrors f <*> checkErrors x
checkErrors (Lam ann v b) = Lam ann v <$> checkErrors b
checkErrors (Var ann v)   = pure $ Var ann v
checkErrors (Prim ann p)  = pure $ Prim ann p
checkErrors (Lit ann l)   = pure $ Lit ann l
checkErrors (Err ann err) = Failure ((ann, err) :| [])
