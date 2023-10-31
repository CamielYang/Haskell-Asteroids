{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Models.StateMonad where

import           Control.Monad
import           Models.Model

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure x' = S $ \s -> (x', s)
  (<*>) = ap
instance Monad (State s) where
  return = pure
  (S f) >>= g = S $ \s -> let (x', s') = f s in
                          let S g' = g x' in
                          g' s'

runState :: State s a -> s -> (a, s)
runState (S f) = f
