{-# LANGUAGE ScopedTypeVariables #-}
import qualified Codec.Picture       as JP
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Word           (Word8)
import           Numeric             (readHex)
import qualified Options.Applicative as OA

data Color = Color Word8 Word8 Word8 deriving (Show)

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
    { oColors   :: [Color]
    , oFilePath :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.many (OA.option (OA.eitherReader colorReader)
            (OA.long "color" <> OA.short 'c' <> OA.metavar "rrggbb"))
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
            case M.lookup p mapColors of
                Just (Color r g b) -> JP.PixelRGBA8 r g b 255
                _                  -> p)
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
    img <- JP.readImage (oFilePath args) >>= either fail pure
    result <- either fail pure $ recolor (oColors args) (JP.convertRGBA8 img)
    JP.savePngImage (oFilePath args) $ JP.ImageRGBA8 $ result
  where
    opts = OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc
