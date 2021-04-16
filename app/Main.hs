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
import System.IO
import Types

runOne :: Env -> Text -> IO ()
runOne env input = do
  let parsed = parseExpr input
  case parsed of
    Left err -> print err
    Right x -> do
      print =<< runIOThrows (eval env x)

runRepl :: Env -> IO ()
runRepl env =
  forever do
    putStr "> "
    hFlush stdout
    input <- getLine
    runOne env . T.pack $ input
    pure ()

main :: IO ()
main = do
  xs <- System.Environment.getArgs
  init <- newIORef M.empty
  case length xs of
    1 -> do
      runOne init . T.pack . head $ xs
    _ ->
      runRepl init
