{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Expr
    ( Expr (..)
    , getAnn
    , mapAnn
    , mapErr
    , freeVars
    , allVars
    , normalizeVars
    , checkCycles
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
    | Lit ann Integer
    | Id ann (Expr ann e v)
    | Err ann e
    deriving (Eq, Foldable, Functor, Show)

getAnn :: Expr a e v -> a
getAnn (App ann _ _) = ann
getAnn (Lam ann _ _) = ann
getAnn (Var ann _)   = ann
getAnn (Prim ann _)  = ann
getAnn (Lit ann _)   = ann
getAnn (Id ann _)    = ann
getAnn (Err ann _)   = ann

mapAnn :: (a -> b) -> Expr a e v -> Expr b e v
mapAnn m (App ann f x) = App  (m ann) (mapAnn m f) (mapAnn m x)
mapAnn m (Lam ann v b) = Lam  (m ann) v (mapAnn m b)
mapAnn m (Var ann v)   = Var  (m ann) v
mapAnn m (Prim ann p)  = Prim (m ann) p
mapAnn m (Lit ann l)   = Lit  (m ann) l
mapAnn m (Id ann e)    = Id   (m ann) (mapAnn m e)
mapAnn m (Err ann err) = Err  (m ann) err

mapErr :: (err0 -> err1) -> Expr ann err0 v -> Expr ann err1 v
mapErr m (App ann f x) = App  ann (mapErr m f) (mapErr m x)
mapErr m (Lam ann v b) = Lam  ann v (mapErr m b)
mapErr _ (Var ann v)   = Var  ann v
mapErr _ (Prim ann p)  = Prim ann p
mapErr _ (Lit ann l)   = Lit  ann l
mapErr m (Id ann e)    = Id   ann (mapErr m e)
mapErr m (Err ann err) = Err  ann (m err)

-- | Free variables in an expression.
freeVars :: Ord v => Expr ann e v -> S.Set v
freeVars (App _ f x)    = freeVars f <> freeVars x
freeVars (Lam _ v body) = S.delete v $ freeVars body
freeVars (Var _ v)      = S.singleton v
freeVars (Prim _ _)     = S.empty
freeVars (Lit _ _)      = S.empty
freeVars (Id _ e)       = freeVars e
freeVars (Err _ _)      = S.empty

-- | All variables in an expression.
allVars :: Ord v => Expr ann e v -> S.Set v
allVars = foldMap S.singleton

-- | Only works if all variables are bound.
normalizeVars :: forall ann e v. Ord v => Expr ann e v -> Expr ann e Int
normalizeVars expr = evalState (go expr) (0, M.empty)
  where
    go :: Expr ann e v -> State (Int, M.Map v Int) (Expr ann e Int)
    go (App ann f x) = App ann <$> go f <*> go x
    go (Lam ann v b) = Lam ann <$> var v <*> go b
    go (Var ann v)   = Var ann <$> var v
    go (Prim ann p)  = pure $ Prim ann p
    go (Lit ann l)   = pure $ Lit ann l
    go (Id ann e)    = Id ann <$> go e
    go (Err ann e)   = pure $ Err ann e

    var :: v -> State (Int, M.Map v Int) Int
    var v = state $ \(fresh, vars) -> case M.lookup v vars of
        Nothing -> (fresh, (fresh + 1, M.insert v fresh vars))
        Just n  -> (n, (fresh, vars))

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
        _ | ann `S.member` visited -> Err ann (mkError expr)
        App _ f x                  -> App ann (go visited' f) (go visited' x)
        Lam _ v b                  -> Lam ann v $ go visited' b
        Id _ e                     -> Id ann (go visited' e)
        _                          -> expr
      where
        ann      = getAnn expr
        visited' = S.insert ann visited

-- | Removes errors from an expression.
checkErrors
    :: Ord ann
    => Expr ann e v -> Validation (NonEmpty (ann, e)) (Expr ann Void v)
checkErrors = \expr -> case collect S.empty expr of
    []         -> Success $ unsafeErr expr
    err : errs -> Failure (err :| errs)
  where
    collect visited expr = case expr of
        Err _ e   -> [(ann, e)]
        _ | ann `S.member` visited -> []
        App _ f x -> collect visited' f ++ collect visited' x
        Lam _ _ b -> collect visited' b
        Var _ _   -> []
        Prim _ _  -> []
        Lit _ _   -> []
        Id _ e    -> collect visited' e
      where
        ann      = getAnn expr
        visited' = S.insert ann visited

    unsafeErr (App ann f x) = App ann (unsafeErr f) (unsafeErr x)
    unsafeErr (Lam ann v b) = Lam ann v (unsafeErr b)
    unsafeErr (Var ann v)   = Var ann v
    unsafeErr (Prim ann p)  = Prim ann p
    unsafeErr (Lit ann l)   = Lit ann l
    unsafeErr (Id ann e)    = Id ann (unsafeErr e)
    unsafeErr (Err _ _)     = error "checkErrors: error left after check"
