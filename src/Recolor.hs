{-# LANGUAGE FlexibleContexts    #-}
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
import           Turnstyle.TwoD

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

exprToConstraints
    :: forall img err. (Image img, Ord (Pixel img))
    => img -> Expr Ann err (Pixel img) -> Either err [ColorConstraint Pos]
exprToConstraints img = exprToConstraints' img S.empty M.empty

exprToConstraints'
    :: forall img err. (Image img, Ord (Pixel img))
    => img
    -> S.Set Ann
    -> M.Map (Pixel img) Pos
    -> Expr Ann err (Pixel img)
    -> Either err [ColorConstraint Pos]
exprToConstraints' img visited ctx expr
    | Err _ err <- expr      = Left err
    | ann `S.member` visited = pure []
    | otherwise              = (constraints ++) <$> children
  where
    ann      = getAnn expr
    visited' = S.insert ann visited
    children = case expr of
        Lam _ _ b -> exprToConstraints' img visited' ctx' b
        App _ lhs rhs -> (++)
            <$> exprToConstraints' img visited' ctx' lhs
            <*> exprToConstraints' img visited' ctx' rhs
        Id _ e -> exprToConstraints' img visited' ctx' e
        _ -> pure []

    relPixel rel = let (Pos px py) = relPos pos dir rel in pixel px py img
    (pos, dir, quattern) = ann

    ctx' = case quattern of
        AABC -> M.insert (relPixel RightPos)  r ctx
        ABCB -> M.insert (relPixel LeftPos)   l ctx
        ABBC -> M.insert (relPixel CenterPos) c ctx
        _    -> ctx

    constraints = case quattern of
        AAAA -> [Eq l c, Eq l f, Eq l r]
        AAAB -> [Eq l c, Eq l f, NotEq l r] ++
            [Eq (ctx M.! relPixel RightPos) r]
        AABA -> [Eq l c, NotEq l f, Eq l r] ++
            [Eq (ctx M.! relPixel FrontPos) f]
        AABB -> [Eq l c, NotEq l f, Eq f r]
        AABC -> [Eq l c, NotEq l f, NotEq l r, NotEq f r]
        ABAA -> [NotEq l c, Eq l f, Eq l r] ++
            [Eq (ctx M.! relPixel CenterPos) c]
        ABAB -> [NotEq l c, Eq l f, Eq c r]
        ABAC -> [NotEq l c, Eq l f, NotEq l r, NotEq c r]
        ABBA -> [NotEq l c, Eq c f, Eq l r]
        ABBB -> [NotEq l c, Eq c f, Eq c r] ++
            [Eq (ctx M.! relPixel LeftPos) l]
        ABBC -> [NotEq l c, Eq c f, NotEq l r, NotEq c r]
        ABCA -> [NotEq l c, NotEq l f, NotEq c f, Eq l r]
        ABCB -> [NotEq l c, NotEq l f, NotEq c f, Eq c r]
        ABCC -> [NotEq l c, NotEq l f, NotEq c f, Eq f r]
        ABCD ->
            [NotEq l c, NotEq l f, NotEq c f, NotEq l r, NotEq c r, NotEq f r] ++
            contiguousConstrains l ++
            contiguousConstrains f ++
            contiguousConstrains r

    l = relPos pos dir LeftPos
    c = relPos pos dir CenterPos
    f = relPos pos dir FrontPos
    r = relPos pos dir RightPos

    contiguousConstrains p =
        let inside = contiguous p img
            border = S.fromList $
                filter (not . (`S.member` inside)) $
                concatMap neighbors (S.toList inside) in
        [Eq p i | i <- S.toList inside, i /= p] ++
        [NotEq p b | b <- S.toList border]

main :: IO ()
main = do
    args <- OA.execParser opts
    result <- case oRecompile args of
        True -> do
            img <- loadImage $ oFilePath args
            let expr = parseImage Nothing img
            constrs <- either (fail . show) pure $ exprToConstraints img expr
            solution <- either (fail . show) pure $ solve constrs
            let shape = Shape
                    { sWidth       = width img
                    , sHeight      = height img
                    , sEntrance    = height img `div` 2
                    , sConstraints = constrs
                    }
            pure $ paint (map colorToPixel $ oColors args) solution shape
        False -> do
            img <- JP.readImage (oFilePath args) >>= either fail pure
            either fail pure $ recolor (oColors args) (JP.convertRGBA8 img)
    JP.savePngImage (oFilePath args) $ JP.ImageRGBA8 $ result
  where
    opts = OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc
