{-# LANGUAGE OverloadedStrings #-}
import qualified Codec.Picture                as JP
import qualified Data.ByteString.Lazy         as BL
import           Data.Foldable                (for_)
import           Data.List                    (isSuffixOf)
import           Numeric                      (showHex)
import           System.Directory             (listDirectory)
import           System.FilePath              (dropExtension, (</>))
import qualified System.IO                    as IO
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Text.Blaze.Svg11             as Svg
import qualified Text.Blaze.Svg11.Attributes  as A
import qualified Text.Blaze.Html5.Attributes as HA

annotate :: Int -> JP.Image JP.PixelRGBA8 -> Svg.Svg
annotate factor ref = Svg.docTypeSvg
        Svg.! A.version "1.1"
        Svg.! A.width (Svg.toValue width)
        Svg.! A.height (Svg.toValue height) $ linky $ do

    for_ [0 .. JP.imageHeight ref - 1] $ \y ->
        for_ [0 .. JP.imageWidth ref - 1] $ \x -> do
            case JP.pixelAt ref x y of
                JP.PixelRGBA8 _ _ _ 0 -> mempty :: Svg.Svg
                JP.PixelRGBA8 r g b _ -> Svg.rect
                    Svg.! A.x (Svg.toValue (x * factor))
                    Svg.! A.y (Svg.toValue (y * factor))
                    Svg.! A.width (Svg.toValue factor)
                    Svg.! A.height (Svg.toValue factor)
                    Svg.! A.fill (Svg.toValue $ hexColor r g b)

    let cx = factor `div` 2
        cy = initialY * factor + factor `div` 2
        cr = factor `div` 6
        tw = cr
    Svg.circle
        Svg.! A.cx (Svg.toValue cx)
        Svg.! A.cy (Svg.toValue cy)
        Svg.! A.r (Svg.toValue cr)
        Svg.! A.fill "#000"
    Svg.line
        Svg.! A.x1 (Svg.toValue cx)
        Svg.! A.y1 (Svg.toValue cy)
        Svg.! A.x2 (Svg.toValue (cx + factor))
        Svg.! A.y2 (Svg.toValue cy)
        Svg.! A.stroke "#000"
        Svg.! A.strokeWidth "2"
    let triangle =
            [ (cx + factor - tw, cy - tw)
            , (cx + factor + tw, cy)
            , (cx + factor - tw, cy + tw)
            ]
    Svg.polygon
        Svg.! A.points (Svg.toValue $ unwords
            [show x ++ "," ++ show y | (x, y) <- triangle])
        Svg.! A.stroke "#000"
  where
    width  = factor * JP.imageWidth ref
    height = factor * JP.imageHeight ref

    hexColor r g b = "#" ++ hexWord r ++ hexWord g ++ hexWord b
    hexWord w = case showHex w "" of
        [h] -> ['0', h]
        h   -> h

    initialY = JP.imageHeight ref `div` 2

    linky b = Svg.a Svg.! HA.href "examples/minimal.png" $ b

main :: IO ()
main = do
    examples <- filter (not . (".svg" `isSuffixOf`)) <$>
        listDirectory "examples"
    for_ examples $ \example -> do
        let path = "examples" </> example
        IO.hPutStrLn IO.stderr $ "Enlarging " ++ path ++ "..."
        img <- JP.readImage path >>= either fail pure
        let base = dropExtension example
        BL.writeFile ("examples" </> base ++ ".svg") $ renderSvg $
            annotate 20 $ JP.convertRGBA8 img
