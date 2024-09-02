import qualified Codec.Picture               as JP
import           Data.Either.Validation      (Validation (..))
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Data.Word                   (Word8)
import           Numeric                     (readHex)
import qualified Options.Applicative         as OA
import           Turnstyle.Compile.Paint     (paint)
import           Turnstyle.Compile.Recompile (recompile)
import           Turnstyle.Compile.Shape     (Shape (..))
import           Turnstyle.Compile.Solve     (solve)
import           Turnstyle.Expr              (checkErrors)
import           Turnstyle.Image
import           Turnstyle.JuicyPixels       (loadImage)
import           Turnstyle.Parse

data Color = Color Word8 Word8 Word8 deriving (Show)

colorToPixel :: Color -> JP.PixelRGBA8
colorToPixel (Color r g b) = JP.PixelRGBA8 r g b 255

colorReader :: String -> Either String Color
colorReader (r0 : r1 : g0 : g1 : b0 : b1 : []) =
    maybe (Left "could not parse color") Right $
    Color <$> parseHex [r0, r1] <*> parseHex [g0, g1] <*> parseHex [b0, b1]
  where
    parseHex str = case readHex str of
        [(n, "")] -> Just n
        _         -> Nothing
colorReader _ = Left "expected rrggbb"

data Options = Options
    { oColors    :: [Color]
    , oRecompile :: Bool
    , oFilePath  :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.many (OA.option (OA.eitherReader colorReader)
            (OA.long "color" <> OA.short 'c' <> OA.metavar "rrggbb"))
    <*> OA.switch (OA.long "recompile")
    <*> OA.argument OA.str (OA.metavar "IMAGE.PNG")

recolor
    :: [Color] -> JP.Image JP.PixelRGBA8
    -> Either String (JP.Image JP.PixelRGBA8)
recolor colors ref
    | S.size refColors > length colors = Left $
        "not enough colors (need " ++ show (S.size refColors) ++ ")"
    | otherwise = Right $ JP.generateImage
        (\x y ->
            let p = JP.pixelAt ref x y in
            maybe p colorToPixel (M.lookup p mapColors))
        (JP.imageWidth ref)
        (JP.imageHeight ref)
  where
    mapColors = M.fromList $ zip (S.toList refColors) colors

    refColors = S.fromList $ do
        x <- [0 .. JP.imageWidth ref - 1]
        y <- [0 .. JP.imageHeight ref - 1]
        case JP.pixelAt ref x y of
            JP.PixelRGBA8 _ _ _ 0 -> []
            p                     -> [p]

main :: IO ()
main = do
    args <- OA.execParser opts
    result <- case oRecompile args of
        True -> do
            img <- loadImage $ oFilePath args
            expr <- case checkErrors (parseImage Nothing img) of
                Success e   -> pure e
                Failure err -> fail $ show err
            let constrs = recompile img expr
                palette = map colorToPixel $ oColors args
            solution <- either (fail . show) pure $ solve palette constrs
            let shape = Shape
                    { sWidth       = width img
                    , sHeight      = height img
                    , sEntrance    = height img `div` 2
                    , sConstraints = constrs
                    }
            pure $ paint solution shape
        False -> do
            img <- JP.readImage (oFilePath args) >>= either fail pure
            either fail pure $ recolor (oColors args) (JP.convertRGBA8 img)
    JP.savePngImage (oFilePath args) $ JP.ImageRGBA8 $ result
  where
    opts = OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc
