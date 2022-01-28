module Fallible (Fallible, FallibleIO, ExceptT (..), err, assert, correct, wrap, unwrap, dropLeft, assertJust) where

import Control.Monad.Except (ExceptT (..))

type Fallible a = Either String a
type FallibleIO a = ExceptT String IO a

err :: Integer -> String -> Fallible a
err line msg = Left $ "Line " ++ show (line + 1) ++ ": " ++ msg

assert :: Bool -> Integer -> String -> Fallible ()
assert True _ _ = Right ()
assert False line msg = err line msg

assertJust :: Maybe a -> Integer -> String -> Fallible a
assertJust (Just a) _ _ = Right a
assertJust Nothing line msg = err line msg

dropLeft :: Fallible a -> Maybe a
dropLeft (Right x) = Just x
dropLeft (Left _) = Nothing

correct :: Monad m => m a -> ExceptT e m a
correct = ExceptT . (>>= return . Right)

wrap :: Monad m => Either e a -> ExceptT e m a
wrap = ExceptT . return

unwrap :: Monad m => (e -> r) -> (a -> r) -> ExceptT e m a -> m r
unwrap left right (ExceptT except) = fmap extract except
  where extract (Left e) = left e
        extract (Right a) = right a
