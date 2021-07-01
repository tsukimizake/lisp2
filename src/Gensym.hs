module Gensym
  ( GensymT,
    gensym,
    runGensymT,
  )
where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.State
import Data.Functor
import Data.Text as T

type GensymT m a = StateT Int m a

gensym :: (Monad m) => GensymT m Text
gensym = do
  count <- get
  let count' = count + 1
  put count'
  pure $ T.pack $ show count'

runGensymT :: (Monad m) => GensymT m a -> m a
runGensymT = flip evalStateT 0
