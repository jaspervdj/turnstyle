module Turnstyle.Prim
    ( Prim (..)
    , InMode (..)
    , OutMode (..)
    , NumOpMode (..)
    , CmpMode (..)
    , InexactMode (..)

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
    | PInexact InexactMode
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
    | NumOpModulo
    | NumOpFloor
    | NumOpCeil
    deriving (Bounded, Enum, Eq, Show)

data CmpMode
    = CmpEq
    | CmpLessThan
    | CmpGreaterThan
    | CmpLessThanOrEqual
    | CmpGreaterThanOrEqual
    deriving (Bounded, Enum, Eq, Show)

data InexactMode
    = InexactSqrt
    deriving (Bounded, Enum, Eq, Show)

knownPrims :: [Prim]
knownPrims =
    map PIn      [minBound .. maxBound] <>
    map POut     [minBound .. maxBound] <>
    map PNumOp   [minBound .. maxBound] <>
    map PCompare [minBound .. maxBound] <>
    map PInexact [minBound .. maxBound]

primArity :: Prim -> Int
primArity (PIn      _)           = 2
primArity (POut     _)           = 2
primArity (PNumOp   NumOpFloor)  = 1
primArity (PNumOp   NumOpCeil)   = 1
primArity (PNumOp   _)           = 2
primArity (PCompare _)           = 4
primArity (PInexact InexactSqrt) = 1

primName :: Prim -> String
primName (PIn InNumber)                   = "in_num"
primName (PIn InChar)                     = "in_char"
primName (POut OutNumber)                 = "out_num"
primName (POut OutChar)                   = "out_char"
primName (PNumOp NumOpAdd)                = "num_add"
primName (PNumOp NumOpSubtract)           = "num_sub"
primName (PNumOp NumOpMultiply)           = "num_mul"
primName (PNumOp NumOpDivide)             = "num_div"
primName (PNumOp NumOpModulo)             = "num_mod"
primName (PNumOp NumOpFloor)              = "num_floor"
primName (PNumOp NumOpCeil)               = "num_ceil"
primName (PCompare CmpEq)                 = "cmp_eq"
primName (PCompare CmpLessThan)           = "cmp_lt"
primName (PCompare CmpGreaterThan)        = "cmp_gt"
primName (PCompare CmpLessThanOrEqual)    = "cmp_lte"
primName (PCompare CmpGreaterThanOrEqual) = "cmp_gte"
primName (PInexact InexactSqrt)           = "inexact_sqrt"

encodePrim :: Prim -> (Int, Int)
encodePrim (PIn InNumber)                   = (1, 1)
encodePrim (PIn InChar)                     = (1, 2)
encodePrim (POut OutNumber)                 = (2, 1)
encodePrim (POut OutChar)                   = (2, 2)
encodePrim (PNumOp NumOpAdd)                = (3, 1)
encodePrim (PNumOp NumOpSubtract)           = (3, 2)
encodePrim (PNumOp NumOpMultiply)           = (3, 3)
encodePrim (PNumOp NumOpDivide)             = (3, 4)
encodePrim (PNumOp NumOpModulo)             = (3, 5)
encodePrim (PNumOp NumOpFloor)              = (3, 6)
encodePrim (PNumOp NumOpCeil)               = (3, 7)
encodePrim (PCompare CmpEq)                 = (4, 1)
encodePrim (PCompare CmpLessThan)           = (4, 2)
encodePrim (PCompare CmpGreaterThan)        = (4, 3)
encodePrim (PCompare CmpLessThanOrEqual)    = (4, 4)
encodePrim (PCompare CmpGreaterThanOrEqual) = (4, 5)
encodePrim (PInexact InexactSqrt)           = (5, 1)

decodeMap :: M.Map (Int, Int) Prim
decodeMap = M.fromList [(encodePrim p, p) | p <- knownPrims]

decodePrim :: Int -> Int -> Maybe Prim
decodePrim modul opcode = M.lookup (modul, opcode) decodeMap
