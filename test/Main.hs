import           Test.Tasty               (defaultMain, testGroup)
import qualified Turnstyle.Compile.Tests
import qualified Turnstyle.Quattern.Tests

main :: IO ()
main = defaultMain $ testGroup "Turnstyle"
    [ Turnstyle.Compile.Tests.tests
    , Turnstyle.Quattern.Tests.tests
    ]
