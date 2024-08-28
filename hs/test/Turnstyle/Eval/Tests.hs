{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Turnstyle.Eval.Tests
    ( tests
    ) where

import           Control.Monad.Except  (ExceptT, runExceptT, throwError)
import           Control.Monad.State   (State, modify, runState, state)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Turnstyle.Eval
import           Turnstyle.JuicyPixels (loadImage)
import           Turnstyle.Number
import           Turnstyle.Parse       (parseImage)
import           Turnstyle.Scale       (autoScale)

data EvalState = EvalState
    { esInNumbers  :: [Integer]
    , esInChars    :: [Char]
    , esOutNumbers :: [Number]
    , esOutChars   :: [Char]
    }

emptyEvalState :: EvalState
emptyEvalState = EvalState [] [] [] []

newtype EvalPure a =
    EvalPure {unEvalPure :: ExceptT EvalException (State EvalState) a}
    deriving (Applicative, Functor, Monad)

instance MonadEval EvalPure where
    evalThrow = EvalPure . throwError
    evalInputNumber = EvalPure $ state $ \es -> case esInNumbers es of
        []     -> (Nothing, es)
        x : xs -> (Just x, es {esInNumbers = xs})
    evalInputChar = EvalPure $ state $ \es -> case esInChars es of
        []     -> (Nothing, es)
        x : xs -> (Just x, es {esInChars = xs})
    evalOutputNumber n = EvalPure $ modify $ \es ->
        es {esOutNumbers = n : esOutNumbers es}
    evalOutputChar n = EvalPure $ modify $ \es ->
        es {esOutChars = n : esOutChars es}

tests :: TestTree
tests = testGroup "Turnstyle.Eval"
    [ testCase "examples/pi.png" $ do
        img <- autoScale <$> loadImage "examples/pi.png"
        let expr = parseImage Nothing img
            (result, finalState) =
                runState (runExceptT $ unEvalPure $ eval expr) emptyEvalState
        result @?= Right (Lit 0)
        esOutNumbers finalState @?= [Exact $ 5284 / 1681]
    ]
