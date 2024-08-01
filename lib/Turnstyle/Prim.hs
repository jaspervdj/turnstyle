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
    | InLine
    | InChar
    deriving (Bounded, Enum, Eq, Show)

data OutMode
    = OutNumber
    | OutLine
    | OutChar
    deriving (Bounded, Enum, Eq, Show)

data NumOpMode
    = NumOpAdd
    | NumOpSubtract
    | NumOpMultiply
    | NumOpDivide
    | NumOpModulo
    deriving (Bounded, Enum, Eq, Show)

data CmpMode
    = CmpEq
    | CmpLessThan
    | CmpGreaterThan
    | CmpLessThanOrEqual
    | CmpGreaterThanOrEqual
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
primName (PIn InNumber)                   = "in_num"
primName (PIn InLine)                     = "in_line"
primName (PIn InChar)                     = "in_char"
primName (POut OutNumber)                 = "out_num"
primName (POut OutLine)                   = "out_line"
primName (POut OutChar)                   = "out_char"
primName (PNumOp NumOpAdd)                = "num_add"
primName (PNumOp NumOpSubtract)           = "num_sub"
primName (PNumOp NumOpMultiply)           = "num_mul"
primName (PNumOp NumOpDivide)             = "num_div"
primName (PNumOp NumOpModulo)             = "num_mod"
primName (PCompare CmpEq)                 = "cmp_eq"
primName (PCompare CmpLessThan)           = "cmp_lt"
primName (PCompare CmpGreaterThan)        = "cmp_gt"
primName (PCompare CmpLessThanOrEqual)    = "cmp_lte"
primName (PCompare CmpGreaterThanOrEqual) = "cmp_gte"

encodePrim :: Prim -> (Int, Int)
encodePrim (PIn InNumber)                   = (1, 1)
encodePrim (PIn InLine)                     = (1, 2)
encodePrim (PIn InChar)                     = (1, 3)
encodePrim (POut OutNumber)                 = (2, 1)
encodePrim (POut OutLine)                   = (2, 2)
encodePrim (POut OutChar)                   = (2, 3)
encodePrim (PNumOp NumOpAdd)                = (3, 1)
encodePrim (PNumOp NumOpSubtract)           = (3, 2)
encodePrim (PNumOp NumOpMultiply)           = (3, 3)
encodePrim (PNumOp NumOpDivide)             = (3, 4)
encodePrim (PNumOp NumOpModulo)             = (3, 5)
encodePrim (PCompare CmpEq)                 = (4, 1)
encodePrim (PCompare CmpLessThan)           = (4, 2)
encodePrim (PCompare CmpGreaterThan)        = (4, 3)
encodePrim (PCompare CmpLessThanOrEqual)    = (4, 4)
encodePrim (PCompare CmpGreaterThanOrEqual) = (4, 5)

decodeMap :: M.Map (Int, Int) Prim
decodeMap = M.fromList [(encodePrim p, p) | p <- knownPrims]

decodePrim :: Int -> Int -> Maybe Prim
decodePrim modul opcode = M.lookup (modul, opcode) decodeMap
