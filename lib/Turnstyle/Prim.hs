module Turnstyle.Prim
    ( Prim (..)
    , InMode (..)
    , OutMode (..)
    , NumOpMode (..)
    , CmpMode (..)

    , knownPrims
    , primArity
    , primName
    , decodePrim
    , encodePrim
    ) where

import qualified Data.Map as M

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
    = CmpEq
    | CmpLessThan
    | CmpGreaterThan
    deriving (Bounded, Enum, Eq, Show)

knownPrims :: [Prim]
knownPrims =
    map PIn      [minBound .. maxBound] <>
    map POut     [minBound .. maxBound] <>
    map PNumOp   [minBound .. maxBound] <>
    map PCompare [minBound .. maxBound]

primArity :: Prim -> Int
primArity (PIn      _) = 2
primArity (POut     _) = 2
primArity (PNumOp   _) = 2
primArity (PCompare _) = 4

primName :: Prim -> String
primName (PIn InNumber)            = "in_num"
primName (PIn InChar)              = "in_char"
primName (POut OutNumber)          = "out_num"
primName (POut OutChar)            = "out_char"
primName (PNumOp NumOpAdd)         = "num_add"
primName (PNumOp NumOpSubtract)    = "num_sub"
primName (PNumOp NumOpMultiply)    = "num_mul"
primName (PNumOp NumOpDivide)      = "num_div"
primName (PCompare CmpEq)          = "cmp_eq"
primName (PCompare CmpLessThan)    = "cmp_lt"
primName (PCompare CmpGreaterThan) = "cmp_gt"

encodePrim :: Prim -> (Int, Int)
encodePrim (PIn InNumber)            = (1, 0)
encodePrim (PIn InChar)              = (1, 1)
encodePrim (POut OutNumber)          = (2, 0)
encodePrim (POut OutChar)            = (2, 1)
encodePrim (PNumOp NumOpAdd)         = (3, 0)
encodePrim (PNumOp NumOpSubtract)    = (3, 1)
encodePrim (PNumOp NumOpMultiply)    = (3, 2)
encodePrim (PNumOp NumOpDivide)      = (3, 3)
encodePrim (PCompare CmpEq)          = (4, 0)
encodePrim (PCompare CmpLessThan)    = (4, 1)
encodePrim (PCompare CmpGreaterThan) = (4, 2)

decodeMap :: M.Map (Int, Int) Prim
decodeMap = M.fromList [(encodePrim p, p) | p <- knownPrims]

decodePrim :: Int -> Int -> Maybe Prim
decodePrim opcode mode = M.lookup (opcode, mode) decodeMap
