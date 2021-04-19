module EvalSpec where

import Eval
import Parser
import Test.Hspec

spec :: Spec
spec = describe "eval" do
  it "can eval" do
    1 `shouldBe` 1
