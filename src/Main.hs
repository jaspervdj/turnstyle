{-# LANGUAGE TypeApplications #-}
module Main (main) where

import qualified Codec.Picture           as JP
import           Data.Either.Validation  (Validation (..))
import qualified Options.Applicative     as OA
import           Text.Read               (readMaybe)
import           Turnstyle.Compile.Paint
import           Turnstyle.Compile.Shape
import           Turnstyle.Eval          (eval)
import           Turnstyle.Expr
import           Turnstyle.JuicyPixels   (loadImage)
import           Turnstyle.Parse         (Pos (..), parseImage)
import           Turnstyle.Pretty        (prettyExpr)
import           Turnstyle.Prim
import           Turnstyle.Scale         (autoScale)

data Options = Options
    { oCommand         :: Command
    } deriving (Show)

data Command
    = Run RunOptions
    | Compile
    deriving (Show)

data RunOptions = RunOptions
    { roInitialPosition :: Maybe Pos
    , roFilePath        :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.subparser
            (OA.command "run"
                (OA.info (Run <$> parseRunOptions)
                (OA.progDesc "Run turnstyle program")) <>
            OA.command "compile"
                (OA.info (pure Compile)
                (OA.progDesc "Compile lambda expression to image")))

parseRunOptions :: OA.Parser RunOptions
parseRunOptions = RunOptions
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
    case oCommand args of
        Run ropts -> do
            img <- loadImage $ roFilePath ropts
            let expr = parseImage (roInitialPosition ropts) (autoScale img)
            putStrLn $ prettyExpr expr
            case checkErrors expr of
                Failure _ -> putStrLn "errors in expression"
                Success e -> eval e >>= print
        Compile -> do
            JP.savePngImage "a.png" $ JP.ImageRGB8 $
                paint $ exprToShape @Int $
                    -- Lit () 3
                    -- Prim () $ PNumOp NumOpAdd
                    Lam () 1 $ Lit () 3
  where
    opts = OA.info (parseOptions OA.<**> OA.helper)
        (OA.fullDesc <> OA.progDesc "turnstyle interpreter")
