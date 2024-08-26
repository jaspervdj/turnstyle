{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Turnstyle.Compile.Constraint
    ( ColorConstraint (..)
    ) where

data ColorConstraint p
    = Eq p p
    | NotEq p p
    deriving (Foldable, Functor, Show)
