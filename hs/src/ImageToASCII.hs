import qualified Codec.Picture       as JP
import qualified Data.Text.IO        as T
import qualified Options.Applicative as OA
import           Turnstyle.Image

data Options = Options
    { oFilePath  :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.argument OA.str (OA.metavar "IMAGE.PNG")

main :: IO ()
main = do
    args <- OA.execParser opts
    img <- fmap JP.convertRGBA8 $
        JP.readImage (oFilePath args) >>= either fail pure
    T.putStr $ asciiImageToText $ imageToAsciiImage ['A' .. 'Z'] img
  where
    opts = OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc
