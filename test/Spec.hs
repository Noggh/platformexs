import           BobSpec             (bobSpecs)
import           LuhnSpec            (luhnSpecs)
import           WordCountSpec       (wordCountSpecs)
import           Test.Hspec          (Spec)
import           Test.Hspec.Runner   (configFailFast, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} mainSpecs

mainSpecs :: Spec
mainSpecs = do
    bobSpecs
    luhnSpecs
    wordCountSpecs
