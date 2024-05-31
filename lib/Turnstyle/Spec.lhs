This is the specification of the Turnstyle language.

> {-# LANGUAGE TypeFamilies #-}
> module Turnstyle.Spec
>   ( Image (..)
>   , example
>   ) where

> class Image img where
>   type Pixel img
>   width  :: img -> Int
>   height :: img -> Int
>   pixel  :: Int -> Int -> img -> Pixel img

> data Expr a
>   = Literal Int
>   | App (Expr a) (Expr a)
>   | Lit Int
>   | Lam a (Expr a)
>   | Var a
>   deriving (Show)

> data Loc
>   = FrontLeft
>   | FrontRight
>   | BackRight
>   | BackLeft
>   deriving (Eq, Show)

> data Dir = U | R | D | L deriving (Eq, Show)

> loc :: Int -> Int -> Dir -> Loc -> (Int, Int)
> loc x y U FrontLeft  = (x,     y    )
> loc x y U FrontRight = (x + 1, y    )
> loc x y U BackLeft   = (x,     y + 1)
> loc x y U BackRight  = (x + 1, y + 1)
> loc x y R FrontLeft  = (x + 1, y    )
> loc x y R FrontRight = (x + 1, y + 1)
> loc x y R BackLeft   = (x,     y    )
> loc x y R BackRight  = (x,     y + 1)
> loc x y D FrontLeft  = (x + 1, y + 1)
> loc x y D FrontRight = (x    , y + 1)
> loc x y D BackLeft   = (x + 1, y    )
> loc x y D BackRight  = (x,     y    )
> loc x y L FrontLeft  = (x    , y + 1)
> loc x y L FrontRight = (x    , y    )
> loc x y L BackLeft   = (x + 1, y + 1)
> loc x y L BackRight  = (x + 1, y    )

> parse :: Image img => Int -> Int -> Dir -> img -> Expr (Pixel img)
> parse = undefined

> example :: IO ()
> example = putStrLn "Hi Turnstyle"
