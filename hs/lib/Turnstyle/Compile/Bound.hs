module Turnstyle.Compile.Bound
    ( checkVars
    ) where

import           Data.Foldable        (toList)
import qualified Data.Set             as S
import           Turnstyle.Text.Sugar

-- | Checks that all variables are bound.
checkVars :: Sugar err ann -> [(ann, String)]
checkVars = go S.empty
  where
    go vars (Let _ v d b)  = go vars d <> go (S.insert v vars) b
    go _    (Import _ _ _) = []
    go vars (App _ f xs)   = go vars f <> foldMap (go vars) xs
    go vars (Lam _ vs b)   = go (vars <> S.fromList (toList vs)) b
    go vars (Var ann v)    = if S.member v vars then [] else [(ann, v)]
    go _    (Prim _ _)     = []
    go _    (Lit _ _)      = []
    go _    (Err _ _)      = []
