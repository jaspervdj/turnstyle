{-# LANGUAGE MultiWayIf #-}
module Turnstyle.Quattern
    ( Quattern (..)
    , quattern
    ) where

-- | A Quattern is a quad-pattern, a pattern consisting of four elements
-- arranged in TurnStyle order.
data Quattern
    = AAAA
    | AAAB
    | AABA
    | AABB
    | AABC
    | ABAA
    | ABAB
    | ABAC
    | ABBA
    | ABBB
    | ABBC
    | ABCA
    | ABCB
    | ABCC
    | ABCD
    deriving (Bounded, Enum, Eq, Show)

quattern :: Eq a => a -> a -> a -> a -> Quattern
quattern a x y z = if
   | x == a    -> if | y == a    -> if | z == a    -> AAAA
                                       | otherwise -> AAAB
                     | otherwise -> if | z == a    -> AABA
                                       | z == y    -> AABB
                                       | otherwise -> AABC
   | otherwise -> if | y == a    -> if | z == a    -> ABAA
                                       | z == x    -> ABAB
                                       | otherwise -> ABAC
                     | y == x    -> if | z == a    -> ABBA
                                       | z == x    -> ABBB
                                       | otherwise -> ABBC
                     | otherwise -> if | z == a    -> ABCA
                                       | z == x    -> ABCB
                                       | z == y    -> ABCC
                                       | otherwise -> ABCD
