module Fallible (Fallible (..), FallibleT (..), success, failure, assert, wrap) where

import Control.Monad (liftM, ap)

data Fallible a = Ok a | Error String

instance Monad Fallible where
  return = Ok
  Ok a >>= f = f a
  Error s >>= _ = Error s

failure :: Integer -> String -> Fallible a
failure line msg = Error $ "Line " ++ show (line + 1) ++ ": " ++ msg

assert :: Bool -> Integer -> String -> Fallible ()
assert True _ _ = Ok ()
assert False line msg = failure line msg

newtype FallibleT m a = FallibleT { runFallibleT :: m (Fallible a) }

instance Monad m => Monad (FallibleT m) where
  return = FallibleT . return . return
  x >>= f = FallibleT $ do
    v <- runFallibleT x
    case v of
      Error x -> return $ Error x
      Ok y  -> runFallibleT $ f y

success :: Monad m => m a -> FallibleT m a
success = FallibleT . (>>= return . Ok)

wrap :: Monad m => Fallible a -> FallibleT m a
wrap = FallibleT . return

instance Functor Fallible where
  fmap = liftM

instance Monad m => Functor (FallibleT m) where
  fmap = liftM

instance Applicative Fallible where
  pure = return
  (<*>) = ap

instance Monad m => Applicative (FallibleT m) where
  pure = return
  (<*>) = ap
