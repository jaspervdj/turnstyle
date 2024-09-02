{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Turnstyle.Compile.Constraint
    ( ColorConstraint (..)
    ) where

data ColorConstraint c p
    = Eq p p
    | NotEq p p
    | LitEq c p
    deriving (Foldable, Functor, Show)
