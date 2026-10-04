{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Turnstyle.Eval
    ( EvalIO (..)
    , defaultEvalIO
    , EvalError (..)
    , Whnf (..)
    , whnf
    , eval
    , typeOf
    ) where

import           Control.Exception (IOException, handle)
import           Data.Char         (chr, ord)
import           Data.IORef        (IORef, atomicModifyIORef, newIORef,
                                    writeIORef)
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

data EvalError ann err v
    = SourceError ann err
    | UnboundVar ann v
    | NotFunction ann Type
    | PrimBadArg Prim Int Type Type
    | DivideByZero
    | CyclicThunk
    deriving (Eq)

instance (Show ann, Show err, Show v) => Show (EvalError ann err v) where
    show (SourceError ann err) =
        show ann ++ ": error in expression: " ++ show err
    show (UnboundVar ann v) =
        show ann ++ ": unbound variable: " ++ show v
    show (NotFunction ann t) =
        show ann ++ ": cannot use " ++ show t ++ " as a function"
    show (PrimBadArg p n expected actual) =
        "primitive " ++ primName p ++ " bad argument #" ++ show n ++
        ": expected " ++ show expected ++ " but got " ++ show actual
    show DivideByZero = "division by zero"
    show CyclicThunk = "cyclic thunk evaluation"

data EvalIO = EvalIO
    { evalInputNumber  :: IO (Maybe Integer)
    , evalInputChar    :: IO (Maybe Char)
    , evalOutputNumber :: Number -> IO ()
    , evalOutputChar   :: Char -> IO ()
    }

defaultEvalIO :: EvalIO
defaultEvalIO = EvalIO
    { evalInputNumber = handle @IOException (\_ -> pure Nothing) (Just <$> readLn)
    , evalInputChar = handle @IOException (\_ -> pure Nothing) (Just <$> getChar)
    , evalOutputNumber = print
    , evalOutputChar   = putChar
    }

eval
    :: (Expr.PosAnn ann, Ord v)
    => EvalIO -> Expr ann err v -> IO (Whnf ann err v)
eval io = whnf io (Env M.empty)

newtype Env ann err v = Env {unEnv :: M.Map v (Thunk ann err v)}

newtype Thunk ann err v = Thunk (IORef (ThunkBody ann err v))

data ThunkBody ann err v
    = ExprThunk (Env ann err v) (Expr ann err v)
    | WhnfThunk (Whnf ann err v)
    | WorkThunk

-- | An expression in WHNF.
data Whnf ann err v
    = Lam v (Env ann err v) (Expr ann err v)
    | UnsatPrim Int Prim [Thunk ann err v]
    | Lit Number
    | Err (EvalError ann err v)

whnf
    :: (Expr.PosAnn ann, Ord v)
    => EvalIO -> Env ann err v -> Expr ann err v -> IO (Whnf ann err v)
whnf io env (Expr.App ann f x) = do
    fv <- whnf io env f
    let envX = Env $ M.filterWithKey (\k _ -> k `S.member` Expr.freeVars x) $ unEnv env
    xThunk <- Thunk <$> newIORef (ExprThunk envX x)
    whnfApp io ann fv xThunk
whnf _ env (Expr.Lam _ v body) = do
    let lamEnv = Env $ M.filterWithKey (\k _ -> k `S.member` Expr.freeVars body) $ unEnv env
    pure $ Lam v lamEnv body
whnf io env (Expr.Var ann v) = case M.lookup v (unEnv env) of
    Nothing    -> pure $ Err $ UnboundVar ann v
    Just thunk -> whnfThunk io thunk
whnf _ _ (Expr.Prim _ p) = pure $ UnsatPrim (primArity p) p []
whnf _ _ (Expr.Lit _ lit) = pure $ Lit $ fromIntegral lit
whnf io env (Expr.Id _ e) = whnf io env e
whnf _ _ (Expr.Err ann err) = pure $ Err $ SourceError ann err

whnfApp
    :: (Expr.PosAnn ann, Ord v)
    => EvalIO -> ann -> Whnf ann err v -> Thunk ann err v -> IO (Whnf ann err v)
whnfApp io ann f x = case f of
    Lam v lamEnv body -> do
        let env' = Env $ M.insert v x $ unEnv lamEnv
        whnf io env' body
    UnsatPrim 1 p args -> prim io ann p (reverse (x : args))
    UnsatPrim n p args -> pure $ UnsatPrim (n - 1) p (x : args)
    Err err            -> pure $ Err err
    _                  -> pure $ Err $ NotFunction ann (typeOf f)

whnfThunk
    :: (Expr.PosAnn ann, Ord v) => EvalIO -> Thunk ann err v
    -> IO (Whnf ann err v)
whnfThunk io (Thunk ref) = do
    body <- atomicModifyIORef ref $ \body -> case body of
        ExprThunk _ _ -> (WorkThunk, body)
        WhnfThunk _   -> (body, body)
        WorkThunk     -> (body, body)
    case body of
        WorkThunk -> pure $ Err CyclicThunk
        WhnfThunk x -> pure x
        ExprThunk env expr -> do
            x <- whnf io env expr
            writeIORef ref (WhnfThunk x)
            pure x

prim
    :: (Expr.PosAnn ann, Ord v)
    => EvalIO -> ann -> Prim -> [Thunk ann err v] -> IO (Whnf ann err v)
prim io ann (PIn inMode) [k, l] = do
    mbLit <- case inMode of
        InNumber -> evalInputNumber io
        InChar   -> fmap (fromIntegral . ord) <$> evalInputChar io
    case mbLit of
        Nothing  -> whnfThunk io l
        Just lit -> do
            fv <- whnfThunk io k
            litThunk <- Thunk <$> newIORef (WhnfThunk (Lit (fromIntegral lit)))
            whnfApp io ann fv litThunk
prim io _ p@(POut outMode) [outE, kE] =
    castArgNumber io p 1 outE $ \out ->
        case outMode of
            OutNumber -> do
                evalOutputNumber io out
                whnfThunk io kE
            OutChar -> case numberToInt out of
                Nothing -> pure $ Err $ PrimBadArg p 1 IntegralTy NumberTy
                Just n  -> do
                    evalOutputChar io $ chr n
                    whnfThunk io kE
prim io _ p@(PNumOp numOp) [xE, yE] =
    castArgNumber io p 1 xE $ \x ->
    castArgNumber io p 2 yE $ \y -> case numOp of
        NumOpAdd             -> pure $ Lit $ x + y
        NumOpSubtract        -> pure $ Lit $ x - y
        NumOpDivide | y == 0 -> pure $ Err DivideByZero
        NumOpDivide          -> pure $ Lit $ x / y
        NumOpMultiply        -> pure $ Lit $ x * y
        NumOpModulo          -> do
            case numberToInteger x of
                Nothing -> pure $ Err $ PrimBadArg p 1 IntegralTy NumberTy
                Just xi -> case numberToInteger y of
                    Nothing -> pure $ Err $ PrimBadArg p 2 IntegralTy NumberTy
                    Just yi -> pure $ Lit $ fromInteger $ xi `mod` yi
prim io _ p@(PCompare cmp) [xE, yE, fE, gE] =
    castArgNumber io p 1 xE $ \x ->
    castArgNumber io p 2 yE $ \y ->
    case cmp of
        CmpEq                 -> whnfThunk io $ if x == y then fE else gE
        CmpLessThan           -> whnfThunk io $ if x < y  then fE else gE
        CmpGreaterThan        -> whnfThunk io $ if x > y  then fE else gE
        CmpLessThanOrEqual    -> whnfThunk io $ if x <= y then fE else gE
        CmpGreaterThanOrEqual -> whnfThunk io $ if x >= y then fE else gE
prim io _ p@(PInexact InexactSqrt) [xE] =
    castArgNumber io p 1 xE $ \x ->
        pure . Lit . Inexact . sqrt $ numberToDouble x

castArgNumber
    :: (Expr.PosAnn ann, Ord v)
    => EvalIO -> Prim -> Int -> Thunk ann err v
    -> (Number -> IO (Whnf ann err v)) -> IO (Whnf ann err v)
castArgNumber io p narg t k = do
    v <- whnfThunk io t
    case v of
        Lit x -> k x
        Err e -> pure $ Err e
        _     -> pure $ Err $ PrimBadArg p narg NumberTy (typeOf v)

typeOf :: Whnf err ann v -> Type
typeOf (Lam _ _ _)       = LambdaTy
typeOf (UnsatPrim _ _ _) = PrimitiveTy
typeOf (Lit _)           = NumberTy
typeOf (Err _)           = ErrorTy
