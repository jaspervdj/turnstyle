{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Turnstyle.Eval.Tests
    ( MockEvalInput (..)
    , emptyMockEvalInput
    , MockEvalOutput (..)
    , mockEvalIO
    , eventually
    , tests
    ) where

import           Control.Concurrent    (forkIO, killThread, threadDelay)
import           Control.Monad         (unless, void)
import           Data.IORef            (atomicModifyIORef, newIORef, readIORef)
import           Data.List             (isPrefixOf)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (Assertion, assertFailure, testCase,
                                        (@?=))
import           Turnstyle.Eval
import           Turnstyle.Image       (textToAsciiImage)
import           Turnstyle.JuicyPixels (loadImage)
import           Turnstyle.Number
import           Turnstyle.Parse       (parseImage)
import           Turnstyle.Scale       (autoScale)

data MockEvalInput = MockEvalInput
    { mockEvalInNumbers :: [Integer]
    , mockEvalInChars   :: [Char]
    } deriving (Show)

emptyMockEvalInput :: MockEvalInput
emptyMockEvalInput = MockEvalInput [] []

data MockEvalOutput = MockEvalOutput
    { mockEvalOutNumbers :: [Number]
    , mockEvalOutChars   :: [Char]
    } deriving (Show)

mockEvalIO :: MockEvalInput -> IO (EvalIO, IO MockEvalOutput)
mockEvalIO input0 = do
    inputRef <- newIORef input0
    outputRef <- newIORef $ MockEvalOutput [] []
    let evalIO = EvalIO
            { evalInputNumber = atomicModifyIORef inputRef $ \input ->
                case mockEvalInNumbers input of
                    []     -> (input, Nothing)
                    x : xs -> (input {mockEvalInNumbers = xs}, Just x)
            , evalInputChar = atomicModifyIORef inputRef $ \input ->
                case mockEvalInChars input of
                    []     -> (input, Nothing)
                    x : xs -> (input {mockEvalInChars = xs}, Just x)
            , evalOutputNumber = \x -> atomicModifyIORef outputRef $ \o ->
                (o {mockEvalOutNumbers = x : mockEvalOutNumbers o}, ())
            , evalOutputChar = \x -> atomicModifyIORef outputRef $ \o ->
                (o {mockEvalOutChars = x : mockEvalOutChars o}, ())
            }

    pure (evalIO, readIORef outputRef)

tests :: TestTree
tests = testGroup "Turnstyle.Eval"
    [ testCase "examples/pi.png" $ do
        img <- autoScale <$> loadImage "examples/pi.png"
        let expr = parseImage Nothing img

        (evalIO, evalOutputIO) <- mockEvalIO emptyMockEvalInput
        result <- eval evalIO expr
        case result of
            Lit 0 -> pure ()
            _     -> assertFailure "expected 0"
        outputNumbers <- mockEvalOutNumbers <$> evalOutputIO
        outputNumbers @?= [Exact $ 5284 / 1681]

    , testCase "examples/loop.png" $ do
        img <- autoScale <$> loadImage "examples/loop.png"
        let expr = parseImage Nothing img
        (evalIO, evalOutputIO) <- mockEvalIO emptyMockEvalInput
        evalThread <- forkIO $ void $ eval evalIO expr
        eventually "outputs [1, 2, 3]" $ do
            outputNumbers <- reverse . mockEvalOutNumbers <$> evalOutputIO
            pure $
                length outputNumbers >= 3 &&
                [1, 2, 3] `isPrefixOf` outputNumbers
        killThread evalThread

    , testCase "examples/rev.png" $ do
        img <- autoScale <$> loadImage "examples/rev.png"
        let expr = parseImage Nothing img
        (evalIO, evalOutputIO) <- mockEvalIO emptyMockEvalInput
            { mockEvalInChars = "hello\nworld\n"
            }
        result <- eval evalIO expr
        case result of
            Lit 0 -> pure ()
            _     -> assertFailure "expected 0"
        outputChars <- reverse . mockEvalOutChars <$> evalOutputIO
        outputChars @?= "olleh\ndlrow\n"

    , testCase "lifthrasiir" $ do
        img <- either fail pure $ textToAsciiImage
            "......\n\
            \CADABB\n\
            \>>BBCD\n\
            \CBDCAD\n\
            \AAABBD\n"
        let expr = parseImage Nothing img
        result <- eval defaultEvalIO expr
        case result of
            Lit 5 -> pure ()
            _     -> assertFailure "expected 5"
    ]

eventually :: String -> IO Bool -> Assertion
eventually description mx = go 0 1000
  where
    maxAttempts = 20 :: Int

    go attempts _ | attempts > maxAttempts = assertFailure $
        description ++ ": gave up after " ++ show maxAttempts ++ " attempts"
    go attempts delay = do
        x <- mx
        unless x $ do
            threadDelay delay
            go (attempts + 1) (delay * 2)
