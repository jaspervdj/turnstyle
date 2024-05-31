import           Test.Tasty               (defaultMain, testGroup)
import qualified Turnstyle.Quattern.Tests

main :: IO ()
main = defaultMain $ testGroup "Turnstyle"
    [ Turnstyle.Quattern.Tests.tests
    ]
