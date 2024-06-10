import qualified Codec.Picture    as JP
import           Data.Foldable    (for_)
import           Data.List        (isSuffixOf)
import           System.Directory (listDirectory)
import           System.FilePath  (dropExtension, (</>))
import qualified System.IO        as IO

enlarge :: JP.Pixel p => JP.Image p -> JP.Image p
enlarge ref = JP.generateImage
    (\x y -> JP.pixelAt ref (x `div` factor) (y `div` factor))
    (JP.imageWidth ref * factor)
    (JP.imageHeight ref * factor)
  where
    factor = 20

main :: IO ()
main = do
    examples <- filter (not . ("-large.png" `isSuffixOf`)) <$>
        listDirectory "examples"
    for_ examples $ \example -> do
        let path = "examples" </> example
        IO.hPutStrLn IO.stderr $ "Enlarging " ++ path ++ "..."
        img <- JP.readImage path >>= either fail pure
        let base = dropExtension example
        JP.savePngImage ("examples" </> base ++ "-large.png") $
            JP.ImageRGBA8 $ enlarge $ JP.convertRGBA8 img
