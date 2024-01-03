import           BobSpec           (bobSpecs)
import           LuhnSpec          (luhnSpecs)
import           RunLengthSpec     (runLengthSpecs)
import           Test.Hspec        (Spec)
import           Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import           WordCountSpec     (wordCountSpecs)
import           WordySpec         (wordySpecs)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} mainSpecs

mainSpecs :: Spec
mainSpecs = do
    bobSpecs
    luhnSpecs
    runLengthSpecs
    wordCountSpecs
    wordySpecs
