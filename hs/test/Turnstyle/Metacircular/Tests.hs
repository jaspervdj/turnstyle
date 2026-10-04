{-# LANGUAGE OverloadedStrings #-}
module Turnstyle.Metacircular.Tests
    ( tests
    ) where

import           Control.Concurrent    (forkIO, killThread)
import           Control.Monad         (void)
import           Data.List             (isPrefixOf)
import qualified Data.Text             as T
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertFailure, testCase)
import           Turnstyle.Compile
import           Turnstyle.Eval        (eval)
import           Turnstyle.Eval.Tests  (MockEvalInput (..), MockEvalOutput (..),
                                        emptyMockEvalInput, eventually,
                                        mockEvalIO)
import           Turnstyle.Image
import           Turnstyle.JuicyPixels
import           Turnstyle.Parse
import           Turnstyle.Text        (parseSugar)

tests :: TestTree
tests = testGroup "Turnstyle.Metacircular"
    -- TODO: could use Tasty.withResource to share the compiled version accross
    -- tests
    [ testCase "compile metacircular, use it to run loop" $ do
        -- Compile metacircular to an image and parse it for running.
        let metacircularPath = "examples/metacircular.txt"
        metacircularSource <- readFile metacircularPath
        metacircularSugar <- either (assertFailure . show) pure $
            parseSugar metacircularPath metacircularSource
        metacircularImage <- either (assertFailure . show) pure $
            compile defaultCompileOptions metacircularSugar
        let metacircularExpr = parseImage Nothing $ JuicyPixels $
                crImage metacircularImage

        -- Load loop image and convert to ASCII, so the metacircular interpreter
        -- can load it.
        JuicyPixels loopImage <- loadImage "examples/loop.png"
        let loopAscii = imageToAsciiImage ['A' .. 'Z'] loopImage

        -- Evaluate the metacircular interpreter, which will read the ASCII loop
        -- program from (mocked) standard input.
        (evalIO, evalOutputIO) <- mockEvalIO emptyMockEvalInput
            { mockEvalInChars = T.unpack $ asciiImageToText loopAscii
            }
        evalThread <- forkIO $ void $ eval evalIO metacircularExpr
        eventually "outputs [1, 2, 3]" $ do
            outputNumbers <- reverse . mockEvalOutNumbers <$> evalOutputIO
            pure $
                length outputNumbers >= 3 &&
                [1, 2, 3] `isPrefixOf` outputNumbers
        killThread evalThread
    ]
