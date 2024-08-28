import           Test.Tasty               (defaultMain, testGroup)
import qualified Turnstyle.Compile.Tests
import qualified Turnstyle.Eval.Tests
import qualified Turnstyle.Parse.Tests
import qualified Turnstyle.Quattern.Tests
import qualified Turnstyle.Text.Tests

main :: IO ()
main = defaultMain $ testGroup "Turnstyle"
    [ Turnstyle.Compile.Tests.tests
    , Turnstyle.Eval.Tests.tests
    , Turnstyle.Parse.Tests.tests
    , Turnstyle.Quattern.Tests.tests
    , Turnstyle.Text.Tests.tests
    ]
