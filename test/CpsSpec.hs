module CpsSpec where

import Cps
import Data.Text
import Debug.Trace
import Parser
import Test.Hspec
import Text.Parsec
import Types

testToCps :: Text -> Cps -> Spec
testToCps src expected =
  it (unpack src) do
    case runParser expr () "" src of
      Right parsed -> do
        res <- runIOThrows (Cps.fromExpr parsed (pure . Cps.nop))
        res `shouldBe` Right expected
      Left err -> error $ show err

spec :: Spec
spec = describe "Cps.fromExpr" do
  testToCps
    "(if (< (+ x 2) 10) then (debug \"t\") else (debug \"f\"))"
    ( Add [Id "x", Cps.Constant $ Num 2] (Just "t")
        :>> Lt [Id "t", Cps.Constant $ Num 10] Nothing
          :|> [ DebugLog "t" [] Nothing :>> DebugNop (Bool True),
                DebugLog "f" [] Nothing :>> DebugNop (Bool False)
              ]
    )
