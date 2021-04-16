module Eval where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Types

showText :: (Show a) => a -> Text
showText = T.pack . show

typeError :: Text -> Expr -> Text
typeError expected actual = "type error, " <> expected <> " expected on " <> showText actual

coerceNum :: Expr -> IOThrowsError Int
coerceNum (N v) = pure v
coerceNum x = throwError $ typeError "num" x

coerceBool :: Expr -> IOThrowsError Bool
coerceBool (B v) = pure v
coerceBool e = throwError $ typeError "bool" e

evalInfix :: Env -> (Expr -> a -> IOThrowsError Expr) -> [Expr] -> (Expr -> IOThrowsError a) -> IOThrowsError Expr
evalInfix env op args coerce = do
  if null args
    then throwError "no arg"
    else do
      let (h : t) = args
      foldM
        ( \acc arg -> do
            arg' <- coerce arg
            op acc arg'
        )
        h
        t

evalMath :: Env -> (Int -> Int -> Int) -> [Expr] -> IOThrowsError Expr
evalMath env op args = do
  let op' :: Expr -> Int -> IOThrowsError Expr
      op' l r = do
        l' <- coerceNum l
        pure . Constant . Num $ op l' r
  evalInfix env op' args coerceNum

pattern OpList x xs = (List (x : xs))

pattern QuoteList xs = OpList (Atom "quote") xs

pattern N x = Constant (Num x)

pattern B x = Constant (Bool x)

pattern S x = Constant (Str x)

eval :: Env -> Expr -> IOThrowsError Expr
eval env v@(Constant _) = pure v
eval _ (QuoteList xs) = pure $ List xs
eval env (List [Atom "if", pred, t, f]) = do
  pred' <- eval env pred
  case pred' of
    B False -> eval env f
    _ -> eval env t
eval env (List (Atom "cond" : key : clauses)) =
  evalCond env key clauses
eval env (List []) = do
  pure nil
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List [x]) = do
  eval env x
eval env (OpList x xs) = do
  xs' <- mapM (eval env) xs
  x' <- eval env x
  case x' of
    Atom a -> evalBuiltinOp env a xs'
    x -> throwError $ "TODO: non-atom function " <> showText x
eval env (Atom x) = do
  bound <- isBound env x
  if bound
    then getVar env x
    else pure (Atom x)

-- read/write env

safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f xs = Just $ f xs

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

evalCond :: Env -> Expr -> [Expr] -> IOThrowsError Expr
evalCond env key clauses = do
  key' <- eval env key
  case condHead (\(List (List matches : _)) -> key' `elem` matches) clauses of
    Nothing -> pure nil
    Just (List (cond : body)) -> eval env (List body)
    Just x -> throwError $ "malformed cond " <> showText x

evalCompOp :: Env -> (Int -> Int -> Bool) -> [Expr] -> IOThrowsError Expr
evalCompOp env op args = do
  let op' :: Expr -> Int -> IOThrowsError Expr
      op' l r = do
        l' <- coerceNum l
        pure . B $ op l' r
  evalInfix env op' args coerceNum

isBound :: Env -> Sym -> IOThrowsError Bool
isBound envRef var = do
  env <- liftIO $ readIORef envRef
  pure . isJust $ M.lookup var env

defineVar :: Env -> Text -> Expr -> IOThrowsError Expr
defineVar envRef var value = do
  alreadyDefined <- isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef (M.insert var valueRef env)
      return value

getVar :: Env -> Sym -> IOThrowsError Expr
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ "Getting an unbound variable " <> var)
    (liftIO . readIORef)
    (M.lookup var env)

setVar :: Env -> Sym -> Expr -> IOThrowsError Expr
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ "Setting an unbound variable " <> var)
    (liftIO . flip writeIORef val)
    (M.lookup var env)
  pure val

bindVars :: Env -> [(Sym, Expr)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv :: [(Sym, Expr)] -> M.Map Sym (IORef Expr) -> IO (M.Map Sym (IORef Expr))
    extendEnv bindings env = M.fromList <$> mapM addBinding bindings -- TODO 毎回fromListで詰め替えたくない
    addBinding :: (Sym, Expr) -> IO (Sym, IORef Expr)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

-- builtins

-- predicateが初めて満たされたやつを返す
condHead :: (a -> Bool) -> [a] -> Maybe a
condHead predicate xs =
  safe head $ dropWhile (not . predicate) xs

evalBoolOp :: Env -> (Bool -> Bool -> Bool) -> [Expr] -> IOThrowsError Expr
evalBoolOp env op args = do
  let op' :: Expr -> Bool -> IOThrowsError Expr
      op' l r = do
        l' <- coerceBool l
        pure . B $ op l' r
  evalInfix env op' args coerceBool

evalBuiltinOp :: Env -> Sym -> [Expr] -> IOThrowsError Expr
evalBuiltinOp env "+" args = evalMath env (+) args
evalBuiltinOp env "-" args = evalMath env (-) args
evalBuiltinOp env "*" args = evalMath env (*) args
evalBuiltinOp env "/" args = evalMath env div args -- TODO devide by zero error
evalBuiltinOp env "=" args = evalCompOp env (==) args
evalBuiltinOp env "/=" args = evalCompOp env (/=) args
evalBuiltinOp env "<" args = evalCompOp env (<) args
evalBuiltinOp env ">" args = evalCompOp env (>) args
evalBuiltinOp env "<=" args = evalCompOp env (<=) args
evalBuiltinOp env ">=" args = evalCompOp env (>=) args
evalBuiltinOp env "||" args = evalBoolOp env (||) args
evalBuiltinOp env "symbol?" [Atom _] = pure $ B True
evalBuiltinOp env "symbol?" _ = pure $ B False
evalBuiltinOp env "string?" [Constant (Str _)] = pure $ B True
evalBuiltinOp env "string?" _ = pure $ B False
evalBuiltinOp env "number?" [Constant (Num _)] = pure $ B True
evalBuiltinOp env "number?" _ = pure $ B False
evalBuiltinOp env "car" [List (x : _)] = pure x
evalBuiltinOp env "cdr" [List (_ : xs)] = pure $ List xs
evalBuiltinOp env "symbol-to-string" [Atom x] = pure . S $ x
evalBuiltinOp env "string-to-symbol" [Constant (Str x)] = pure $ Atom x
evalBuiltinOp env "eq?" [x, y] = pure $ B $ x == y
evalBuiltinOp env op args = throwError $ "not implemented: " <> showText op <> " " <> showText args

nil :: Expr
nil = List []
