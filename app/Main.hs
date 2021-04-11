module Main where

import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Eval
import Parser
import Types

main :: IO ()
main = do
  print $ eval M.empty $ OpList (Atom "+") [N 3, N 1, OpList (Atom "*") [N 19, N 2]]
