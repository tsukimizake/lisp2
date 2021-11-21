module CpsSpec where


import Data.Text
import Parser
import Test.Hspec
import Text.Parsec
import Types
import Cps
import Debug.Trace

testToCps :: Text -> Cps -> Expectation
testToCps src expected =
    case runParser expr () "" src of
      Right parsed -> do
        res <- runIOThrows (Cps.fromExpr parsed (pure . Cps.nop))
        res `shouldBe` Right expected
      Left err -> error $ show err

spec :: Spec
spec = describe "Cps.fromExpr" do
  it "compiles if" do
         testToCps "(if (< (+ x 2) 10) then (debug \"t\") else (debug \"f\"))"
          (Add [Id "x", Cps.Constant $ Num 2] (Just "t")
            :>> Lt [Id "t", Cps.Constant $ Num 10] Nothing
              :|> [ DebugLog "t" [] Nothing :>> DebugNop (Bool True),
                    DebugLog "f" [] Nothing :>> DebugNop (Bool False)
                  ])
