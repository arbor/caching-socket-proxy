{-# LANGUAGE MultiParamTypeClasses #-}

module App.AppError
  ( module App.AppError
  , Control.Monad.Except.throwError
  ) where

import Control.Monad.Catch
import Control.Monad.Except

newtype AppError = AppErr String deriving (Show, Eq)

instance Exception AppError

throwErrorAs :: MonadError e' m => (e -> e') -> Either e a -> m a
throwErrorAs f = either (throwError . f) pure
