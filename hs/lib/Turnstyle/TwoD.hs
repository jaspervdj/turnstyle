module Turnstyle.TwoD
    ( Pos (..)
    , neighbors
    , Dir (..)
    , move
    , rotateLeft
    , rotateRight
    ) where

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord, Show)

neighbors :: Pos -> [Pos]
neighbors p = [move 1 d p | d <- [R, D, L, U]]

data Dir = R | D | L | U deriving (Eq, Ord, Show)

move :: Int -> Dir -> Pos -> Pos
move n R (Pos x y) = Pos (x + n) (y    )
move n D (Pos x y) = Pos (x    ) (y + n)
move n L (Pos x y) = Pos (x - n) (y    )
move n U (Pos x y) = Pos (x    ) (y - n)

rotateLeft, rotateRight :: Dir -> Dir
rotateLeft R = U
rotateLeft D = R
rotateLeft L = D
rotateLeft U = L
rotateRight  = rotateLeft . rotateLeft . rotateLeft
