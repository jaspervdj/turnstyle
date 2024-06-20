module Turnstyle.Main
    ( main
    ) where

import qualified Codec.Picture           as JP
import           Data.Either.Validation  (Validation (..))
import           Data.Maybe              (fromMaybe)
import qualified Options.Applicative     as OA
import qualified System.IO               as IO
import           Text.Read               (readMaybe)
import           Turnstyle.Compile.Paint
import           Turnstyle.Compile.Shape
import           Turnstyle.Eval          (eval)
import           Turnstyle.Expr
import           Turnstyle.JuicyPixels   (loadImage)
import           Turnstyle.Parse         (Pos (..), parseImage)
import           Turnstyle.Scale         (autoScale)
import qualified Turnstyle.Text          as Text

data Options = Options
    { oCommand         :: Command
    } deriving (Show)

data Command
    = Run RunOptions
    | Compile CompileOptions
    deriving (Show)

data RunOptions = RunOptions
    { roInitialPosition :: Maybe Pos
    , roFilePath        :: FilePath
    } deriving (Show)

data CompileOptions = CompileOptions
    { coOut      :: Maybe FilePath
    , coFilePath :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.subparser
            (OA.command "run"
                (OA.info (Run <$> parseRunOptions)
                (OA.progDesc "Run turnstyle program")) <>
            OA.command "compile"
                (OA.info (Compile <$> parseCompileOptions)
                (OA.progDesc "Compile lambda expression to image")))

parseRunOptions :: OA.Parser RunOptions
parseRunOptions = RunOptions
    <$> OA.optional (OA.option (OA.eitherReader parsePos)
            (OA.long "initial-position" <> OA.metavar "X,Y" ))
    <*> OA.argument OA.str (OA.metavar "IMAGE.PNG")
  where
    parsePos :: String -> Either String Pos
    parsePos str = maybe (Left "invalid position") Right $
        case break (== ',') str of
            (x, ',' : y) -> Pos <$> readMaybe x <*> readMaybe y
            _            -> Nothing

parseCompileOptions :: OA.Parser CompileOptions
parseCompileOptions = CompileOptions
    <$> OA.optional (OA.strOption
            (OA.long "out" <> OA.short 'o' <> OA.metavar "IMAGE.PNG"))
    <*> OA.argument OA.str (OA.metavar "PROGRAM.TXT")

main :: IO ()
main = do
    args <- OA.execParser opts
    case oCommand args of
        Run ropts -> do
            img <- loadImage $ roFilePath ropts
            let expr = parseImage (roInitialPosition ropts) (autoScale img)
            case checkErrors expr of
                Failure err -> do
                    IO.hPutStrLn IO.stderr "errors in expression"
                    IO.hPutStrLn IO.stderr $ show err
                Success e   -> do
                    putStrLn $ Text.prettyExpr e
                    eval e >>= print
        Compile copts -> do
            let out = fromMaybe "a.png" (coOut copts)
            contents <- readFile $ coFilePath copts
            case Text.parseSugar (coFilePath copts) contents of
                Left err  -> IO.hPutStrLn IO.stderr $ show err
                Right sugar -> do
                    putStrLn $ Text.prettySugar sugar
                    let expr = Text.sugarToExpr sugar
                    JP.savePngImage out $ JP.ImageRGB8 $
                        paint $ exprToShape $ normalizeVars expr
  where
    opts = OA.info (parseOptions OA.<**> OA.helper)
        (OA.fullDesc <> OA.progDesc "Turnstyle")
