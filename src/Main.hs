module Main (main) where

import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import qualified System.IO             as IO
import           Turnstyle.JuicyPixels

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            _ <- loadImage path
            pure ()
        _ -> do
            progName <- getProgName
            IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <image>"
            exitFailure
