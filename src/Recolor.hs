{-# LANGUAGE ScopedTypeVariables #-}
import qualified Codec.Picture           as JP
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Data.Word               (Word8)
import           Numeric                 (readHex)
import qualified Options.Applicative     as OA
import           Turnstyle.Compile.Paint (paint)
import           Turnstyle.Compile.Shape (ColorConstraint (..), Shape (..))
import           Turnstyle.Compile.Solve (solve)
import           Turnstyle.Expr
import           Turnstyle.Image
import           Turnstyle.JuicyPixels   (loadImage)
import           Turnstyle.Parse
import           Turnstyle.Quattern      (Quattern (..))

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

annotations :: Ord ann => Expr ann err v -> Either err [ann]
annotations = go S.empty
  where
    go visited expr
        | Err _ err <- expr      = Left err
        | ann `S.member` visited = pure []
        | otherwise              = (ann :) <$> children
      where
        ann      = getAnn expr
        visited' = S.insert ann visited
        children = case expr of
            Lam _ _ b -> go visited' b
            App _ f x -> (++) <$> go visited' f <*> go visited' x
            Id _ e    -> go visited' e
            _         -> pure []

annToConstraints :: Ann -> [ColorConstraint Pos]
annToConstraints (pos, dir, quattern) = case quattern of
    AAAA -> [Eq l c, Eq l f, Eq l r]
    AAAB -> [Eq l c, Eq l f, NotEq l r]
    AABA -> [Eq l c, NotEq l f, Eq l r]
    AABB -> [Eq l c, NotEq l f, Eq f r]
    AABC -> [Eq l c, NotEq l f, NotEq l r, NotEq f r]
    ABAA -> [NotEq l c, Eq l f, Eq l r]
    ABAB -> [NotEq l c, Eq l f, Eq c r]
    ABAC -> [NotEq l c, Eq l f, NotEq l r, NotEq c r]
    ABBA -> [NotEq l c, Eq c f, Eq l r]
    ABBB -> [NotEq l c, Eq c f, Eq c r]
    ABBC -> [NotEq l c, Eq c f, NotEq l r, NotEq c r]
    ABCA -> [NotEq l c, NotEq l f, NotEq c f, Eq l r]
    ABCB -> [NotEq l c, NotEq l f, NotEq c f, Eq c r]
    ABCC -> [NotEq l c, NotEq l f, NotEq c f, Eq f r]
    ABCD -> [NotEq l c, NotEq l f, NotEq c f, NotEq l r, NotEq c r, NotEq f r]
  where
    l = relPos pos dir LeftPos
    c = relPos pos dir CenterPos
    f = relPos pos dir FrontPos
    r = relPos pos dir RightPos

main :: IO ()
main = do
    args <- OA.execParser opts
    result <- case oRecompile args of
        True -> do
            img <- loadImage $ oFilePath args
            let expr = parseImage Nothing img
            anns <- either (fail . show) pure $ annotations expr
            let shape = Shape
                    { sWidth       = width img
                    , sHeight      = height img
                    , sEntrance    = height img `div` 2
                    , sConstraints = concatMap annToConstraints anns
                    }
            solution <- either (fail . show) pure $ solve $ sConstraints shape
            pure $ paint (map colorToPixel $ oColors args) solution shape
        False -> do
            img <- JP.readImage (oFilePath args) >>= either fail pure
            either fail pure $ recolor (oColors args) (JP.convertRGBA8 img)
    JP.savePngImage (oFilePath args) $ JP.ImageRGBA8 $ result
  where
    opts = OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc
