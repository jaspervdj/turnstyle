{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Turnstyle.Eval
    ( EvalException (..)
    , MonadEval (..)

    , Var (..)
    , Whnf (..)
    , whnf
    , eval
    , HOAS (..)
    , eval2
    ) where

import           Control.Exception (Exception, IOException, handle, throwIO)
import           Control.Monad     ((>=>))
import           Data.Char         (chr, ord)
import qualified Data.Map          as M
import qualified Data.Set          as S
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
    | ErrorTy
    deriving (Eq)

instance Show Type where
    show ApplicationTy = "function application"
    show LambdaTy      = "lambda"
    show VariableTy    = "variable"
    show PrimitiveTy   = "primitive"
    show NumberTy      = "number"
    show IntegralTy    = "integral"
    show ErrorTy       = "error"

data EvalException
    = PrimBadArg Prim Int Type Type
    | DivideByZero
    deriving (Eq)

instance Show EvalException where
    show (PrimBadArg p n expected actual) =
        "primitive " ++ primName p ++ " bad argument #" ++ show n ++
        ": expected " ++ show expected ++ " but got " ++ show actual
    show DivideByZero = "division by zero"

instance Exception EvalException

class Monad m => MonadEval m where
    evalThrow :: EvalException -> m a
    evalInputNumber :: m (Maybe Integer)
    evalInputChar   :: m (Maybe Char)
    evalOutputNumber :: Number -> m ()
    evalOutputChar   :: Char -> m ()

instance MonadEval IO where
    evalThrow = throwIO

    evalInputNumber = handle @IOException (\_ -> pure Nothing) (Just <$> readLn)
    evalInputChar = handle @IOException (\_ -> pure Nothing) (Just <$> getChar)

    evalOutputNumber = print
    evalOutputChar   = putChar

eval :: (MonadEval m, Ord v) => Expr ann err v -> m (Whnf ann err v)
eval = whnf . fmap User

eval2 :: (MonadEval m, Ord v) => Expr ann err v -> m (HOAS m ann err v)
eval2 = hoas_whnf . toHOAS . fmap User

data Var v = User v | Fresh Int deriving (Eq, Ord, Show)

data HOAS m ann err v
    = HApp (HOAS m ann err v) (HOAS m ann err v)
    | HLam (Maybe (Var v)) (HOAS m ann err v -> m (HOAS m ann err v))
    | HVar (Var v)
    | HUnsatPrim Int Prim [HOAS m ann err v]
    | HLit Number
    | HErr ann err

instance (Show err, Show v) => Show (HOAS m ann err v) where
    show (HApp l r) = "(HApp " ++ show l ++ " " ++ show r ++ ")"
    show (HLam Nothing x) = "(HLam \\_ -> _)"
    show (HLam (Just v) x) = "(HLam \\" ++ show v ++ " -> _)"
    show (HVar v) = show v
    show (HUnsatPrim _ _ _) = "UnsatPrim"
    show (HLit n) = show n
    show (HErr _ err) = "(" ++ show err ++ ")"

hoas_nfh :: (MonadEval m, Ord v) => HOAS m ann err v -> m (HOAS m ann err v)
hoas_nfh e@(HVar _) = pure e
hoas_nfh (HLam v b) = pure $ HLam v $ b >=> hoas_nfh
hoas_nfh (HApp f a) = do
    f' <- hoas_whnf f
    case f' of
        HLam _ b -> b a >>= hoas_nfh
        f'       -> HApp <$> hoas_nfh f' <*> hoas_nfh a

hoas_whnf :: (MonadEval m, Ord v) => HOAS m ann err v -> m (HOAS m ann err v)
hoas_whnf (HApp f a) = do
    f' <- hoas_whnf f
    case f' of
        HLam _ b -> b a >>= hoas_whnf
        -- TODO: convert to haskell lambdas instead, fix (undefined :: ann).
        HUnsatPrim 1 p args -> prim2 p (reverse (a : args))
        HUnsatPrim n p args -> pure $ HUnsatPrim (n - 1) p (a : args)
        _                   -> pure $ HApp f' a
hoas_whnf e@(HLam _ _) = pure e
hoas_whnf e@(HVar _) = pure e
hoas_whnf e@(HUnsatPrim 0 p args) = prim2 p (reverse args)  -- unnecessary?
hoas_whnf e@(HUnsatPrim _ _ _) = pure e
hoas_whnf e@(HLit _) = pure e
hoas_whnf e@(HErr _ _) = pure e

toHOAS
    :: forall m ann err v. (Applicative m, Ord v)
    => Expr ann err (Var v) -> HOAS m ann err v
toHOAS = from M.empty
  where
    from
        :: M.Map (Var v) (HOAS m ann err v)
        -> Expr ann err (Var v)
        -> HOAS m ann err v
    from m (Expr.App _ f a)   = HApp (from m f) (from m a)
    from m (Expr.Lam _ v e)   = HLam (Just v) $ \x -> pure $ from (M.insert v x m) e
    from m (Expr.Var _ v)     = maybe (HVar v) id (M.lookup v m)
    from m (Expr.Prim _ p)    = HUnsatPrim (primArity p) p []
    from m (Expr.Lit _ l)     = HLit $ fromIntegral l
    from m (Expr.Id _ e)      = from m e
    from m (Expr.Err ann err) = HErr ann err

-- | An expression in WHNF.
data Whnf ann err v
    = App (Whnf ann err v) (Expr ann err (Var v))
    | Lam (Var v) (Expr ann err (Var v))
    | Var (Var v)
    | UnsatPrim Int Prim [Expr ann err (Var v)]
    | Lit Number
    | Err ann err
    deriving (Eq, Show)

whnf :: (Ord v, MonadEval m) => Expr ann err (Var v) -> m (Whnf ann err v)
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
whnf (Expr.Id _ e) = whnf e
whnf (Expr.Err ann err) = pure $ Err ann err

subst
    :: Ord v
    => Var v -> Expr ann e (Var v) -> Expr ann e (Var v) -> Expr ann e (Var v)
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
    sub (Expr.Id ann e) = Expr.Id ann (sub e)
    sub e@(Expr.Err _ _) = e

    fvs = Expr.freeVars s
    vs  = fvs <> Expr.allVars b

prim
    :: (Ord v, MonadEval m)
    => ann -> Prim -> [Expr ann err (Var v)] -> m (Whnf ann err v)
prim ann (PIn inMode) [k, l] = do
    mbLit <- case inMode of
        InNumber -> evalInputNumber
        InChar   -> fmap (fromIntegral . ord) <$> evalInputChar
    case mbLit of
        Nothing  -> whnf l
        Just lit -> whnf $ Expr.App ann k $ Expr.Lit ann lit
prim _ p@(POut outMode) [outE, kE] = do
    out <- whnf outE >>= castArgNumber p 1
    case outMode of
        OutNumber -> evalOutputNumber out
        OutChar -> case numberToInt out of
            Nothing -> evalThrow $ PrimBadArg p 1 IntegralTy NumberTy
            Just n  -> evalOutputChar $ chr n
    whnf kE
prim _ p@(PNumOp NumOpFloor) [xE] = do
    x <- whnf xE >>= castArgNumber p 1
    pure $ Lit $ fromIntegral $ floor x
prim _ p@(PNumOp NumOpCeil) [xE] = do
    x <- whnf xE >>= castArgNumber p 1
    pure $ Lit $ fromIntegral $ ceiling x
prim _ p@(PNumOp numOp) [xE, yE] = do
    x <- whnf xE >>= castArgNumber p 1
    y <- whnf yE >>= castArgNumber p 2
    fmap Lit $ case numOp of
        NumOpAdd             -> pure $ x + y
        NumOpSubtract        -> pure $ x - y
        NumOpDivide | y == 0 -> evalThrow DivideByZero
        NumOpDivide          -> pure $ x / y
        NumOpMultiply        -> pure $ x * y
        NumOpModulo          -> do
            xi <- maybe (evalThrow (PrimBadArg p 1 IntegralTy NumberTy)) pure $ numberToInteger x
            yi <- maybe (evalThrow (PrimBadArg p 1 IntegralTy NumberTy)) pure $ numberToInteger y
            pure $ fromInteger $ xi `mod` yi
prim _ p@(PCompare cmp) [xE, yE, fE, gE] = do
    x <- whnf xE >>= castArgNumber p 1
    y <- whnf yE >>= castArgNumber p 2
    case cmp of
        CmpEq                 -> if x == y then whnf fE else whnf gE
        CmpLessThan           -> if x < y  then whnf fE else whnf gE
        CmpGreaterThan        -> if x > y  then whnf fE else whnf gE
        CmpLessThanOrEqual    -> if x <= y  then whnf fE else whnf gE
        CmpGreaterThanOrEqual -> if x >= y  then whnf fE else whnf gE
prim _ p@(PInexact InexactSqrt) [xE] = do
    x <- whnf xE >>= castArgNumber p 1
    pure . Lit . Inexact . sqrt $ numberToDouble x

prim2
    :: (Ord v, MonadEval m)
    => Prim -> [HOAS m ann err v] -> m (HOAS m ann err v)
prim2 (PIn inMode) [k, l] = do
    mbLit <- case inMode of
        InNumber -> evalInputNumber
        InChar   -> fmap (fromIntegral . ord) <$> evalInputChar
    case mbLit of
        Nothing  -> hoas_whnf l
        Just lit -> hoas_whnf $ HApp k $ HLit $ fromIntegral lit
prim2 p@(POut outMode) [outE, kE] = do
    out <- hoas_whnf outE >>= hoas_castArgNumber p 1
    case outMode of
        OutNumber -> evalOutputNumber out
        OutChar -> case numberToInt out of
            Nothing -> evalThrow $ PrimBadArg p 1 IntegralTy NumberTy
            Just n  -> evalOutputChar $ chr n
    hoas_whnf kE
prim2 p@(PNumOp NumOpFloor) [xE] = do
    x <- hoas_whnf xE >>= hoas_castArgNumber p 1
    pure $ HLit $ fromIntegral $ floor x
prim2 p@(PNumOp NumOpCeil) [xE] = do
    x <- hoas_whnf xE >>= hoas_castArgNumber p 1
    pure $ HLit $ fromIntegral $ ceiling x
prim2 p@(PNumOp numOp) [xE, yE] = do
    x <- hoas_whnf xE >>= hoas_castArgNumber p 1
    y <- hoas_whnf yE >>= hoas_castArgNumber p 2
    fmap HLit $ case numOp of
        NumOpAdd             -> pure $ x + y
        NumOpSubtract        -> pure $ x - y
        NumOpDivide | y == 0 -> evalThrow DivideByZero
        NumOpDivide          -> pure $ x / y
        NumOpMultiply        -> pure $ x * y
        NumOpModulo          -> do
            xi <- maybe (evalThrow (PrimBadArg p 1 IntegralTy NumberTy)) pure $ numberToInteger x
            yi <- maybe (evalThrow (PrimBadArg p 1 IntegralTy NumberTy)) pure $ numberToInteger y
            pure $ fromInteger $ xi `mod` yi
prim2 p@(PCompare cmp) [xE, yE, fE, gE] = do
    x <- hoas_whnf xE >>= hoas_castArgNumber p 1
    y <- hoas_whnf yE >>= hoas_castArgNumber p 2
    case cmp of
        CmpEq                 -> if x == y  then hoas_whnf fE else hoas_whnf gE
        CmpLessThan           -> if x < y   then hoas_whnf fE else hoas_whnf gE
        CmpGreaterThan        -> if x > y   then hoas_whnf fE else hoas_whnf gE
        CmpLessThanOrEqual    -> if x <= y  then hoas_whnf fE else hoas_whnf gE
        CmpGreaterThanOrEqual -> if x >= y  then hoas_whnf fE else hoas_whnf gE
prim2 p@(PInexact InexactSqrt) [xE] = do
    x <- hoas_whnf xE >>= hoas_castArgNumber p 1
    pure . HLit . Inexact . sqrt $ numberToDouble x

castArgNumber :: MonadEval m => Prim -> Int -> Whnf ann err v -> m Number
castArgNumber p narg e = case e of
    Lit x -> pure x
    _     -> evalThrow $ PrimBadArg p narg NumberTy (typeOf e)

hoas_castArgNumber :: MonadEval m => Prim -> Int -> HOAS m ann err v -> m Number
hoas_castArgNumber p narg e = case e of
    HLit x -> pure x
    _      -> evalThrow $ PrimBadArg p narg NumberTy (hoas_typeOf e)

typeOf :: Whnf err ann v -> Type
typeOf (App _ _)         = ApplicationTy
typeOf (Lam _ _)         = LambdaTy
typeOf (Var _)           = VariableTy
typeOf (UnsatPrim _ _ _) = PrimitiveTy
typeOf (Lit _)           = NumberTy
typeOf (Err _ _)         = ErrorTy

hoas_typeOf :: HOAS m err ann v -> Type
hoas_typeOf (HApp _ _)         = ApplicationTy
hoas_typeOf (HLam _ _)         = LambdaTy
hoas_typeOf (HVar _)           = VariableTy
hoas_typeOf (HUnsatPrim _ _ _) = PrimitiveTy
hoas_typeOf (HLit _)           = NumberTy
hoas_typeOf (HErr _ _)         = ErrorTy
