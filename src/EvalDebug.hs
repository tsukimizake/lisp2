module EvalDebug where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Map as M
import Data.Text
import Eval
import GHC.IO (unsafePerformIO)
import Parser
import Types

makeEnv :: [(Text, Expr)] -> IO Env
makeEnv xs = do
  pairs <- forM xs $ \(param, arg) ->
    do
      argRef <- newIORef arg
      pure (param, argRef)
  newIORef $ M.fromList pairs

traceEnv :: Env -> IO ()
traceEnv e = print =<< showEnv e

traceEval :: Env -> Text -> IO (Either Error Expr)
traceEval env input = do
  case parseExpr input of
    Right expr -> do
      runIOThrows $ eval env expr
    Left err -> error $ show err
