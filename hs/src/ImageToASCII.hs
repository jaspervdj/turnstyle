import qualified Codec.Picture       as JP
import qualified Data.Map            as M
import qualified Options.Applicative as OA

data Options = Options
    { oFilePath  :: FilePath
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.argument OA.str (OA.metavar "IMAGE.PNG")

imageToASCII :: JP.Image JP.PixelRGBA8 -> String
imageToASCII img = go M.empty ['A' .. 'Z'] 0 0
  where
    go :: M.Map JP.PixelRGBA8 Char -> [Char] -> Int -> Int -> String
    go chars fresh x y
        | x >= JP.imageWidth img  = '\n' : go chars fresh 0 (y + 1)
        | y >= JP.imageHeight img = []
        | otherwise               =
            let p = JP.pixelAt img x y in
            case M.lookup p chars of
                Just c -> c : go chars fresh (x + 1) y
                Nothing -> case fresh of
                    (f : fs) -> f : go (M.insert p f chars) fs (x + 1) y
                    []       -> error "ran out of fresh characters"

main :: IO ()
main = do
    args <- OA.execParser opts
    img <- fmap JP.convertRGBA8 $
        JP.readImage (oFilePath args) >>= either fail pure
    putStr $ imageToASCII img
  where
    opts = OA.info (parseOptions OA.<**> OA.helper) OA.fullDesc
