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

annotate :: Int -> JP.Image JP.PixelRGBA8 -> Svg.Svg
annotate factor ref = Svg.docTypeSvg
        Svg.! A.version "1.1"
        Svg.! A.width (Svg.toValue width)
        Svg.! A.height (Svg.toValue height) $ do

    for_ [0 .. JP.imageHeight ref - 1] $ \y ->
        for_ [0 .. JP.imageWidth ref - 1] $ \x -> do
            case JP.pixelAt ref x y of
                JP.PixelRGBA8 _ _ _ 0 -> mempty :: Svg.Svg
                JP.PixelRGBA8 r g b _ -> Svg.rect
                    Svg.! A.x (Svg.toValue (padding + x * factor))
                    Svg.! A.y (Svg.toValue (padding + y * factor))
                    Svg.! A.width (Svg.toValue factor)
                    Svg.! A.height (Svg.toValue factor)
                    Svg.! A.fill (Svg.toValue $ hexColor r g b)

    let cx = padding + factor `div` 2
        cy = padding + initialY * factor + factor `div` 2
        cr = factor `div` 6
        ox = padding
        oy = padding + initialY * factor
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
        Svg.! A.points (pointsValue triangle)
        Svg.! A.fill "#000"
    let outline =
            [ (ox, oy - factor)
            , (ox + factor, oy - factor)
            , (ox + factor, oy)
            , (ox + factor + factor, oy)
            , (ox + factor + factor, oy + factor)
            , (ox + factor, oy + factor)
            , (ox + factor, oy + factor + factor)
            , (ox, oy + factor + factor)
            , (ox, oy - factor)
            ]
    Svg.polyline
        Svg.! A.points (pointsValue outline)
        Svg.! A.stroke "#0008"
        Svg.! A.fill "none"
        Svg.! A.strokeWidth "1"
  where
    padding = factor `div` 10
    width  = factor * JP.imageWidth ref + padding * 2
    height = factor * JP.imageHeight ref + padding * 2

    hexColor r g b = "#" ++ hexWord r ++ hexWord g ++ hexWord b
    hexWord w = case showHex w "" of
        [h] -> ['0', h]
        h   -> h

    initialY = JP.imageHeight ref `div` 2

    pointsValue points = Svg.toValue $ unwords
        [show x ++ "," ++ show y | (x, y) <- points]

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
