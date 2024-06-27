{-# LANGUAGE TypeApplications #-}
module Turnstyle.Eval
    ( Id (..)
    , Whnf (..)
    , whnf
    , eval
    ) where

import           Control.Exception (Exception, IOException, catch, throwIO)
import           Data.Char         (chr, ord)
import qualified Data.Set          as S
import           Data.Void         (Void, absurd)
import qualified Turnstyle.Expr    as Expr
import           Turnstyle.Expr    (Expr)
import           Turnstyle.Number
import           Turnstyle.Prim

data Type
    = ApplicationTy
    | LambdaTy
    | VariableTy
    | PrimitiveTy
    | NumberTy
    | IntegralTy

instance Show Type where
    show ApplicationTy = "function application"
    show LambdaTy      = "lambda"
    show VariableTy    = "variable"
    show PrimitiveTy   = "primitive"
    show NumberTy      = "number"
    show IntegralTy    = "integral"

data EvalException
    = PrimBadArg Prim Int Type Type
    | DivideByZero

instance Show EvalException where
    show (PrimBadArg p n expected actual) =
        "primitive " ++ primName p ++ " bad argument #" ++ show n ++
        ": expected " ++ show expected ++ " but got " ++ show actual
    show DivideByZero = "division by zero"

instance Exception EvalException

eval :: Ord v => Expr ann Void v -> IO (Whnf ann v)
eval = whnf . fmap Id

data Id v = Id v | Fresh Int deriving (Eq, Ord, Show)

-- | An expression in WHNF.
data Whnf ann v
    = App (Whnf ann v) (Expr ann Void (Id v))
    | Lam (Id v) (Expr ann Void (Id v))
    | Var (Id v)
    | UnsatPrim Int Prim [Expr ann Void (Id v)]
    | Lit Number
    deriving (Show)

whnf :: (Ord v) => Expr ann Void (Id v) -> IO (Whnf ann v)
whnf (Expr.App ann f x) = do
    fv <- whnf f
    case fv of
        Lam v body         -> whnf (subst v x body)
        UnsatPrim 1 p args -> prim ann p (reverse (x : args))
        UnsatPrim n p args -> pure $ UnsatPrim (n - 1) p (x : args)
        _                  -> pure $ App fv x
whnf (Expr.Lam _ v body) = pure $ Lam v body
whnf (Expr.Var _ v) = pure $ Var v
whnf (Expr.Prim _ p) = pure $ UnsatPrim (primArity p) p []
whnf (Expr.Lit _ lit) = pure $ Lit $ fromIntegral lit
whnf (Expr.Err _ err) = absurd err

subst
    :: Ord v
    => Id v -> Expr ann e (Id v) -> Expr ann e (Id v) -> Expr ann e (Id v)
subst x s b = sub b
  where
    sub e@(Expr.Var _ v)
        | v == x    = s
        | otherwise = e
    sub e@(Expr.Lam ann v e')
        | v == x = e
        | v `S.member` fvs = Expr.Lam ann v' (sub e'')
        | otherwise = Expr.Lam ann v (sub e')
      where
        v' = head $ filter (`S.notMember` vs) [Fresh n | n <- [0 ..]]
        e'' = subst v (Expr.Var ann v') e'
    sub (Expr.App ann f a) = Expr.App ann (sub f) (sub a)
    sub e@(Expr.Lit _ _) = e
    sub e@(Expr.Prim _ _) = e
    sub e@(Expr.Err _ _) = e

    fvs = Expr.freeVars s
    vs  = fvs <> Expr.allVars b

prim
    :: Ord v => ann -> Prim -> [Expr ann Void (Id v)] -> IO (Whnf ann v)
prim ann (PIn inMode) [k, l] = catch @IOException
    (do
        lit <- case inMode of
            InNumber -> readLn :: IO Int
            InChar   -> ord <$> getChar
        whnf $ Expr.App ann k (Expr.Lit ann lit))
    (\_ -> whnf l)
prim _ p@(POut outMode) [outE, kE] = do
    out <- whnf outE >>= castArgNumber p 1
    case outMode of
        OutNumber -> print out
        OutChar -> case numberToInt out of
            Nothing -> throwIO $ PrimBadArg p 1 IntegralTy NumberTy
            Just n  -> putChar $ chr n
    whnf kE
prim _ p@(PNumOp numOp) [xE, yE] = do
    x <- whnf xE >>= castArgNumber p 1
    y <- whnf yE >>= castArgNumber p 2
    fmap Lit $ case numOp of
        NumOpAdd             -> pure $ x + y
        NumOpSubtract        -> pure $ x - y
        NumOpDivide | y == 0 -> throwIO DivideByZero
        NumOpDivide          -> pure $ x / y
        NumOpMultiply        -> pure $ x * y
        NumOpModulo          -> do
            xi <- maybe (throwIO (PrimBadArg p 1 IntegralTy NumberTy)) pure $ numberToInteger x
            yi <- maybe (throwIO (PrimBadArg p 1 IntegralTy NumberTy)) pure $ numberToInteger y
            pure $ fromInteger $ xi `mod` yi
prim _ p@(PCompare cmp) [xE, yE, fE, gE] = do
    x <- whnf xE >>= castArgNumber p 1
    y <- whnf yE >>= castArgNumber p 2
    case cmp of
        CmpEq          -> if x == y then whnf fE else whnf gE
        CmpLessThan    -> if x < y  then whnf fE else whnf gE
        CmpGreaterThan -> if x > y  then whnf fE else whnf gE

castArgNumber :: Prim -> Int -> Whnf ann v -> IO Number
castArgNumber p narg e = case e of
    Lit x -> pure x
    _     -> throwIO $ PrimBadArg p narg NumberTy (typeOf e)

typeOf :: Whnf ann v -> Type
typeOf (App _ _)         = ApplicationTy
typeOf (Lam _ _)         = LambdaTy
typeOf (Var _)           = VariableTy
typeOf (UnsatPrim _ _ _) = PrimitiveTy
typeOf (Lit _)           = NumberTy
