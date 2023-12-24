import           BobSpec             (bobSpecs)
import           LuhnSpec            (luhnSpecs)
import           Exercism.HelloWorld (hello)
import           Test.Hspec          (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner   (configFailFast, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} mainSpecs

mainSpecs :: Spec
mainSpecs = do
    bobSpecs
    luhnSpecs
