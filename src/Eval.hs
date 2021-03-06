module Eval where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Types

typeError :: Text -> Expr -> Text
typeError expected actual = "type error, " <> expected <> " expected on " <> showText actual

coerceNum :: Expr -> EvalM Int
coerceNum (N v) = pure v
coerceNum x = throwError $ typeError "num" x

coerceBool :: Expr -> EvalM Bool
coerceBool (B v) = pure v
coerceBool e = throwError $ typeError "bool" e

evalInfix :: (Expr -> a -> EvalM Expr) -> [Expr] -> (Expr -> EvalM a) -> EvalM Expr
evalInfix op args coerce = do
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

evalMath :: Maybe Int -> (Int -> Int -> Int) -> [Expr] -> EvalM Expr
evalMath init op args = do
  let op' :: Expr -> Int -> EvalM Expr
      op' l r = do
        l' <- coerceNum l
        pure . Constant . Num $ op l' r
      args' = case init of
        Just x -> N x : args
        _ -> args
  evalInfix op' args' coerceNum

primitives :: M.Map Text Expr
primitives =
  M.fromList . map (\(Prim sym body) -> (sym, Prim sym body)) $
    [ Prim "+" (evalMath Nothing (+)),
      Prim "-" (evalMath Nothing (-)),
      Prim "*" (evalMath Nothing (*)),
      Prim "/" (evalMath Nothing div), -- TODO devide by zero error
      Prim "==" (evalCompOp (==)),
      Prim "/=" (evalCompOp (/=)),
      Prim "<" (evalCompOp (<)),
      Prim ">" (evalCompOp (>)),
      Prim "<=" (evalCompOp (<=)),
      Prim ">=" (evalCompOp (>=)),
      Prim "||" (evalBoolOp (||)),
      Prim "&&" (evalBoolOp (&&)),
      Prim "symbol?" isSym,
      Prim "string?" isStr,
      Prim "number?" isNum,
      Prim "car" \[List (x : _)] -> pure x,
      Prim "cdr" \[List (_ : xs)] -> pure $ List xs,
      Prim "symbol-to-string" \[Atom x] -> pure $ S x,
      Prim "string-to-symbol" \[S x] -> pure $ S x,
      Prim "eq?" \[x, y] -> pure $ B $ x == y
    ]
  where
    isSym =
      \case
        [Atom _] -> pure $ B True
        _ -> pure $ B False
    isNum =
      \case
        [N _] -> pure $ B True
        _ -> pure $ B False
    isStr =
      \case
        [S _] -> pure $ B True
        _ -> pure $ B False

primitiveSyms :: S.Set Text
primitiveSyms = M.keysSet primitives

has :: (Ord a, Eq a) => S.Set a -> a -> Bool
has set val = S.lookupGE val set == Just val

bindVars :: Env -> [(Sym, Expr)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv :: [(Sym, Expr)] -> M.Map Sym (IORef Expr) -> IO (M.Map Sym (IORef Expr))
    extendEnv bindings env = M.union env . M.fromList <$> mapM addBinding bindings
    addBinding :: (Sym, Expr) -> IO (Sym, IORef Expr)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

apply :: Expr -> [Expr] -> EvalM Expr
apply (Atom sym) args | has primitiveSyms sym = applyPrim (fromJust $ M.lookup sym primitives) args
apply (Func params body closure) args =
  if length params /= length args
    then throwError $ "num params invalid " <> showText (length params) <> " " <> showText args
    else do
      env' <- liftIO (bindVars closure $ zip params args)
      evalBody env'
  where
    remainingArgs = drop (length params) args
    evalBody env = last <$> mapM (eval env) body
apply op args = throwError $ "not implemented: " <> showText op <> " " <> showText args

applyPrim :: Expr -> [Expr] -> EvalM Expr
applyPrim (Prim _ op) args = op args
applyPrim e args = error $ "applyPrim on non-prim" <> show e <> " for " <> show args

makeFunc :: (MonadIO m) => Env -> [Expr] -> [Expr] -> m Expr
makeFunc env args body = return $ Func (map showText args) body env

eval :: Env -> Expr -> EvalM Expr
eval env v@(Constant _) = pure v
eval _ (QuoteList xs) = pure $ List xs
eval env x@(Case key clauses) = evalCaseLam env key clauses
eval env (List [Atom "if", pred, t, f]) = do
  pred' <- eval env pred
  case pred' of
    B False -> eval env f
    _ -> eval env t
eval env (List (Atom "case" : key : clauses)) =
  evalCase env key clauses
eval env (List []) = do
  pure nil
eval env (List [Atom "set", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List [Atom "define", List (Atom var : args), form]) =
  makeFunc env args [form] >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeFunc env params body
eval env (OpList "begin" args) =
  last <$> mapM (eval env) args
eval env (OpList x xs) = do
  xs' <- mapM (eval env) xs
  x' <- eval env (Atom x)
  apply x' xs'
eval env (Atom x) = do
  bound <- isBound env x
  if bound
    then getVar env x
    else pure (Atom x)
eval env x = throwError $ "malformed expr " <> showText x

-- read/write env

safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f xs = Just $ f xs

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

evalCaseLam :: Env -> Expr -> [Clause] -> EvalM Expr
evalCaseLam env key clauses = do
  key' <- eval env key
  (`runContT` id) do
    callCC \break -> do
      forM_ clauses \case
        Clause {patFunc, clauseBody} ->
          if patFunc key'
            then do break $ eval env clauseBody
            else pure ()
      pure $ pure nil

evalCase :: Env -> Expr -> [Expr] -> EvalM Expr
evalCase env key clauses = do
  key' <- eval env key
  case condHead (matchClause key') clauses of
    Nothing -> pure nil
    Just (List (cond : body)) -> fromMaybe nil . safe last <$> mapM (eval env) body
    Just x -> throwError $ "malformed cond " <> showText x
  where
    matchClause :: Expr -> Expr -> Bool
    matchClause key (List (List matches : _)) = key `elem` matches
    matchClause key (List (Atom "else" : _)) = True
    matchClause _ _ = False

-- predicate??????????????????????????????????????????
condHead :: (a -> Bool) -> [a] -> Maybe a
condHead predicate xs =
  safe head $ dropWhile (not . predicate) xs

evalCompOp :: (Int -> Int -> Bool) -> [Expr] -> EvalM Expr
evalCompOp op args = do
  let op' :: Expr -> Int -> EvalM Expr
      op' l r = do
        l' <- coerceNum l
        pure . B $ op l' r
  evalInfix op' args coerceNum

isBound :: Env -> Sym -> EvalM Bool
isBound envRef var = do
  env <- liftIO $ readIORef envRef
  pure . isJust $ M.lookup var env

defineVar :: Env -> Text -> Expr -> EvalM Expr
defineVar envRef var value = do
  alreadyDefined <- isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef (M.insert var valueRef env)
      return value

getVar :: Env -> Sym -> EvalM Expr
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ "Getting an unbound variable " <> var)
    (liftIO . readIORef)
    (M.lookup var env)

setVar :: Env -> Sym -> Expr -> EvalM Expr
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ "Setting an unbound variable " <> var)
    (liftIO . flip writeIORef val)
    (M.lookup var env)
  pure val

-- builtins

evalBoolOp :: (Bool -> Bool -> Bool) -> [Expr] -> EvalM Expr
evalBoolOp op args = do
  let op' :: Expr -> Bool -> EvalM Expr
      op' l r = do
        l' <- coerceBool l
        pure . B $ op l' r
  evalInfix op' args coerceBool

nil :: Expr
nil = List []
