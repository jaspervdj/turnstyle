module Main (main) where

import qualified Options.Applicative   as OA
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import qualified System.IO             as IO
import           Text.Read             (readMaybe)
import           Turnstyle.Eval        (eval)
import           Turnstyle.JuicyPixels (loadImage)
import           Turnstyle.Parse       (Pos (..), initialPosition, parseImage)
import           Turnstyle.Pretty      (prettyExpr)
import           Turnstyle.Scale       (autoScale)

data Options = Options
    { oInitialPosition :: Maybe Pos
    , oFilePath        :: FilePath
    }

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.optional (OA.option (OA.eitherReader parsePos)
        (OA.long "initial-position" <> OA.metavar "X,Y" )))
    <*> OA.argument OA.str (OA.metavar "FILE")
  where
    parsePos :: String -> Either String Pos
    parsePos str = maybe (Left "invalid position") Right $
        case break (== ',') str of
            (x, ',' : y) -> Pos <$> readMaybe x <*> readMaybe y
            _            -> Nothing

main :: IO ()
main = do
    args <- OA.execParser opts
    img <- loadImage $ oFilePath args
    let expr = parseImage (oInitialPosition args) (autoScale img)
    putStrLn $ prettyExpr expr
    eval expr >>= print
  where
    opts = OA.info (parseOptions OA.<**> OA.helper)
        (OA.fullDesc <> OA.progDesc "turnstyle interpreter")
