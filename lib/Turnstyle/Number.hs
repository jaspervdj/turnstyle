module Turnstyle.Number
    ( Number (..)
    , numberToDouble
    , numberToInteger
    , numberToInt
    ) where

import           Data.Ratio (denominator, numerator)

data Number = Exact Rational | Inexact Double

instance Show Number where
    show n = case n of
        _ | Just i <- numberToInteger n -> show i
        Exact x -> show (numerator x) ++ " / " ++ show (denominator x)
        Inexact x -> show x

numberToDouble :: Number -> Double
numberToDouble (Inexact x) = x
numberToDouble (Exact x)   = fromRational x

numberToInteger :: Number -> Maybe Integer
numberToInteger (Exact x)
    | denominator x == 1 = Just (numerator x)
    | otherwise          = Nothing
numberToInteger (Inexact x)
    | abs (fromIntegral rounded - x) <= 0.00001 = Just rounded
    | otherwise                                 = Nothing
  where
    rounded = round x

numberToInt :: Number -> Maybe Int
numberToInt = fmap fromIntegral . numberToInteger

unNumber :: (Rational -> a) -> (Double -> a) -> Number -> a
unNumber exact _       (Exact   x) = exact   x
unNumber _     inexact (Inexact x) = inexact x
{-# INLINE unNumber #-}

binop
    :: (Rational -> Rational -> a)
    -> (Double -> Double -> a)
    -> Number -> Number -> a
binop exact inexact = go
  where
    go (Exact   x) (Exact y)   = exact x y
    go (Inexact x) y           = inexact x (numberToDouble y)
    go x           (Inexact y) = inexact (numberToDouble x) y
{-# INLINE binop #-}

instance Eq Number where
    (==) = binop (==) (==)

instance Ord Number where
    (<=) = binop (<=) (<=)

instance Num Number where
    (+) = binop (\x y -> Exact $ x + y) (\x y -> Inexact $ x + y)
    (*) = binop (\x y -> Exact $ x * y) (\x y -> Inexact $ x * y)
    (-) = binop (\x y -> Exact $ x - y) (\x y -> Inexact $ x - y)

    negate = unNumber (Exact . negate) (Inexact . negate)
    abs    = unNumber (Exact . abs   ) (Inexact . abs   )
    signum = unNumber (Exact . signum) (\x -> Exact $ if x < 0 then -1 else 1)

    fromInteger = Exact . fromInteger

instance Fractional Number where
    (/) = binop (\x y -> Exact $ x / y) (\x y -> Inexact $ x / y)
    fromRational = Exact
