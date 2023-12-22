import Test.Hspec        (Spec, it, shouldBe, describe, context)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Exercism.HelloWorld (hello)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
    exercismSpecs

exercismSpecs :: Spec
exercismSpecs = do
    context "Exercism" $ do
        describe "HelloWorld.hello" $ do
            it "is \"Hello, World!\"" $ do
                hello `shouldBe` "Hello, World!"
