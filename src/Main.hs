module Main (main) where

import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import qualified System.IO             as IO
import           Turnstyle.Eval        (eval)
import           Turnstyle.JuicyPixels (loadImage)
import           Turnstyle.Parse       (initialPosition, parseImage)
import           Turnstyle.Pretty      (prettyExpr)
import           Turnstyle.Scale       (autoScale)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            img <- loadImage path
            let expr = parseImage (autoScale img)
            putStrLn $ show $ initialPosition img
            putStrLn $ prettyExpr expr
            eval expr >>= print
        _ -> do
            progName <- getProgName
            IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <image>"
            exitFailure
