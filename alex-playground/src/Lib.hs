{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Alex
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (MonadState, get, state)
import Control.Monad.Trans.State.Strict hiding (get, state)
import Data.Coerce
import Data.Functor.Identity
import Data.Tuple
import Token

{-
  TODO: project idea: a super verbose tokenizer just to see what can be carried around?
  TODO: it seems both GHC and Agda are not using any wrappers - how does that work?
  TODO: find a way to use startcode.
 -}

main :: IO ()
main = print $ runAlex "let xzzz = 1234 in ts" parseAll
  where
    parseAll =
      alexMonadScan >>= \case
        EOF -> pure []
        x -> (x :) <$> parseAll

newtype AlexWrappper a = AW (Alex a)
  deriving
    (Functor, Applicative, Monad)
    via Alex
  deriving
    (MonadError String, MonadState AlexState)
    via StateT' AlexState (Except String)

newtype StateT' s m a = ST' (s -> m (s, a)) deriving (Functor)

from' :: Functor m => StateT' s m a -> StateT s m a
from' (ST' f) = StateT (fmap swap . f)

to' :: Functor m => StateT s m a -> StateT' s m a
to' (StateT f) = ST' (fmap swap . f)

-- instance Functor m => Functor (StateT' s m) where
--  fmap f a = to' $ fmap f (from' a)

instance Monad m => Applicative (StateT' s m) where
  pure a = to' $ from' (pure a)
  f <*> a = to' $ from' f <*> from' a

instance Monad m => Monad (StateT' s m) where
  m >>= f = to' $ from' m >>= \a -> from' $ f a

instance MonadError e m => MonadError e (StateT' s m) where
  throwError a = to' $ from' (throwError a)
  m `catchError` h = to' $ from' m `catchError` (from' . h)

instance Monad m => MonadState s (StateT' s m) where
  state f = ST' $ pure . swap . f

{-
instance MonadError String AlexWrappper where
  throwError e = AW $ Alex (const (Left e))
  (AW (Alex m)) `catchError` h = AW $
    Alex $ \s -> case m s of
      Left l | AW (Alex f) <- h l -> f s
      Right r -> pure r
 -}

{-
  AlexInput is of type (this can be examined in GHCi):

  ( AlexPosn -- position in input?
  , Char -- char before input
  , Data.ByteString.Lazy.Internal.ByteString -- current input?
  , GHC.Int.Int64 -- this is "bpos", looks like byte consumed so far.
  )

 -}
