module EvalSpec where

import Control.Monad.IO.Class
import Data.Text
import Eval
import Parser
import Test.Hspec
import Types
import GHC.IO (unsafePerformIO)

testEval :: Env -> Text -> Expr -> Expectation
testEval env input expect = do
  case parseExpr input of
    Right expr -> do
      evaled <- runIOThrows $ eval env expr
      evaled `shouldBe` Right expect
    Left err -> error $ show err

spec :: Spec
spec = describe "eval" do
  it "can eval" do
    testEval (unsafePerformIO nullEnv) "(+ 1 2 -2 (- 0 1))" (N 0)
