{-# LANGUAGE OverloadedStrings #-}

import           BobSpec             (bobSpecs)
import           Exercism.HelloWorld (hello)
import           Test.Hspec          (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner   (configFailFast, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} mainSpecs

mainSpecs :: Spec
mainSpecs = do
    exercismSpecs
    bobSpecs

exercismSpecs :: Spec
exercismSpecs = do
    describe "hello" $ do
        it "is \"Hello, World!\"" $ do
            hello `shouldBe` "Hello, World!"
