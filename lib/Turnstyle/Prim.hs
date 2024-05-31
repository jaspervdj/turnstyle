module Turnstyle.Prim
    ( Prim (..)
    , InMode (..)
    , OutMode (..)
    , NumOpMode (..)
    , CmpMode (..)

    , primArity
    , decodePrim
    ) where

data Prim
    = PIn      InMode
    | POut     OutMode
    | PNumOp   NumOpMode
    | PCompare CmpMode
    deriving (Eq, Show)

data InMode
    = InNumber
    | InChar
    deriving (Bounded, Enum, Eq, Show)

data OutMode
    = OutNumber
    | OutChar
    deriving (Bounded, Enum, Eq, Show)

data NumOpMode
    = NumOpAdd
    | NumOpSubtract
    | NumOpMultiply
    | NumOpDivide
    deriving (Bounded, Enum, Eq, Show)

data CmpMode
    = CmpLessThan
    deriving (Bounded, Enum, Eq, Show)

primArity :: Prim -> Int
primArity (PIn      _) = 1
primArity (POut     _) = 2
primArity (PNumOp   _) = 2
primArity (PCompare _) = 4

decodePrim :: Int -> Int -> Maybe Prim
decodePrim 1 0 = Just $ PIn InNumber
decodePrim 1 1 = Just $ PIn InChar
decodePrim 2 0 = Just $ POut OutNumber
decodePrim 2 1 = Just $ POut OutChar
decodePrim 3 0 = Just $ PNumOp NumOpAdd
decodePrim 3 1 = Just $ PNumOp NumOpSubtract
decodePrim 3 2 = Just $ PNumOp NumOpMultiply
decodePrim 3 3 = Just $ PNumOp NumOpDivide
decodePrim 4 0 = Just $ PCompare CmpLessThan
decodePrim _ _ = Nothing
