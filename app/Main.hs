module Main where

import Control.Monad
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Eval
import Parser
import System.Environment
import Types

run :: Text -> IO ()
run input = do
  let parsed = parseExpr input
  case parsed of
    Left err -> print err
    Right x -> do
      init <- newIORef M.empty
      print $ eval init x

main :: IO ()
main = do
  xs <- System.Environment.getArgs
  run . T.pack . head $ xs
