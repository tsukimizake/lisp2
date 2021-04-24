module ParserSpec where

import Data.Text
import Parser
import Test.Hspec
import Text.Parsec
import Types

testParser :: Text -> Expr -> Expectation
testParser input expected = do
  runParser expr () "" input `shouldBe` Right expected

spec :: Spec
spec = describe "" do
  it "parse math" do
    testParser "(+ 1)" (List [Atom "+", N 1])
    testParser "(* 1)" (List [Atom "*", N 1])
    testParser "(/ 1)" (List [Atom "/", N 1])
    testParser "(+ +1 -1)" (List [Atom "+", N 1, N (-1)])
  it "parse negative int" do
    testParser "-1" (N (-1))
    testParser "(-1)" (List [N (-1)])
    testParser "(- 1 2)" (List [Atom "-", N 1, N 2])
    testParser "(- a b )" (List [Atom "-", Atom "a", Atom "b"])
