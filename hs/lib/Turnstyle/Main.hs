module Turnstyle.Main
    ( main
    ) where

import qualified Codec.Picture         as JP
import           Data.Foldable         (for_, toList)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Traversable      (for)
import qualified Options.Applicative   as OA
import qualified System.IO             as IO
import           Text.Read             (readMaybe)
import qualified Turnstyle.Compile     as Compile
import           Turnstyle.Eval        (eval)
import           Turnstyle.Expr
import           Turnstyle.JuicyPixels (loadImage)
import           Turnstyle.Parse
import           Turnstyle.Scale       (autoScale)
import qualified Turnstyle.Text        as Text

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
    { coOptimize :: Bool
    , coSeed     :: Maybe Int
    , coOut      :: Maybe FilePath
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
    <$> OA.switch (OA.long "optimize" <> OA.short 'O')
    <*> OA.optional (OA.option OA.auto (OA.long "seed" <> OA.metavar "SEED"))
    <*> OA.optional (OA.strOption
            (OA.long "out" <> OA.short 'o' <> OA.metavar "IMAGE.PNG"))
    <*> OA.argument OA.str (OA.metavar "PROGRAM.TXT")

data Error
    = ParseError ParseError
    | CycleError
    deriving (Show)

main :: IO ()
main = do
    args <- OA.execParser opts
    case oCommand args of
        Run ropts -> do
            for_ [IO.stdin, IO.stdout, IO.stderr] $ \h ->
                IO.hSetBuffering h IO.LineBuffering
            img <- loadImage $ roFilePath ropts
            let expr = parseImage (roInitialPosition ropts) (autoScale img)
            putStrLn $ Text.prettyExpr $ checkCycles (const CycleError) $
                mapErr ParseError expr
            eval expr >>= print
        Compile copts -> do
            let out = fromMaybe "a.png" (coOut copts)
            contents <- readFile $ coFilePath copts
            case Text.parseSugar (coFilePath copts) contents of
                Left err  -> IO.hPutStrLn IO.stderr $ show err
                Right sugar -> do
                    putStrLn $ Text.prettySugar sugar
                    imports <- for (toList $ Text.sugarImports sugar) $ \p -> do
                        img <- loadImage p
                        putStrLn $ "Loaded " ++ p
                        pure (p, img)
                    let compileOptions = Compile.defaultCompileOptions
                            { Compile.coImports  = M.fromList imports
                            , Compile.coOptimize = coOptimize copts
                            }
                    case Compile.compile compileOptions sugar of
                        Left cerr -> IO.hPutStrLn IO.stderr $ show cerr
                        Right img -> JP.savePngImage out $ JP.ImageRGBA8 img
  where
    opts = OA.info (parseOptions OA.<**> OA.helper)
        (OA.fullDesc <> OA.progDesc "Turnstyle")
