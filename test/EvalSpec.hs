module EvalSpec where

import Control.Monad.IO.Class
import Data.IORef
import Data.Map as M
import Data.Text
import Eval
import GHC.IO (unsafePerformIO)
import Parser
import Test.Hspec
import Types

testEval :: Env -> Text -> Expr -> Expectation
testEval env input expect = do
  case parseExpr input of
    Right expr -> do
      evaled <- runIOThrows $ eval env expr
      evaled `shouldBe` Right expect
    Left err -> error $ show err

spec :: Spec
spec = describe "eval" do
  it "can eval math" do
    testEval (unsafePerformIO nullEnv) "(+ 1 2 -2 (- 0 1))" (N 0)
    testEval (unsafePerformIO nullEnv) "(- 2 1)" (N 1)
  it "can eval lambda" do
    testEval (unsafePerformIO nullEnv) "(begin (define (counter inc) (lambda (x) (set inc (+ x inc)) inc)) (define my-count (counter 5)) (my-count 3))" (N 8)
  it "can eval case" do
    testEval (unsafePerformIO nullEnv) "(case 0 ((0) 1))" (N 1)
    testEval (unsafePerformIO nullEnv) "(case 0 ((0) 2) (else 1))" (N 2)
    testEval (unsafePerformIO nullEnv) "(case 1 ((0) 2) (else 1))" (N 1)
  it "can eval fib" do
    testEval (unsafePerformIO nullEnv) "(begin (define (fib n) (case n ((0 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))" (N 89)
