module Functions (pair, (!?), (??), join, joinShow, foldlMapM, tailRecM, tailRec2M, fmapFst) where

import qualified Data.Map.Strict as Map

pair :: a -> b -> (a, b)
pair a b = (a, b)

infixl 9 !?
(!?) :: Integral i => [a] -> i -> Maybe a
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)
_ !? _ = Nothing

infixl 3 ??
(??) :: Maybe a -> a -> a
Nothing ?? x = x
Just x ?? _ = x

join :: (Foldable f) => [c] -> f [c] -> [c]
join str xs
  | null xs = []
  | otherwise = foldr1 (\a b -> a ++ str ++ b) xs

joinShow :: (Show a) => String -> [a] -> String
joinShow str xs = join str (map show xs)

foldlMapM :: (Monad m, Foldable t) => (b -> a -> m (b, c)) -> b -> t a -> m (b, [c])
foldlMapM f seed = foldl combine $ return (seed, [])
  where combine accM item = accM >>= (\(acc, list) -> fmap (\(acc', item') -> (acc', item' : list)) (f acc item))

tailRecM :: (Monad m) => (a -> m Bool) -> (a -> m b) -> (a -> m a) -> a -> m b
tailRecM if' then' else' arg = do
  finish <- if' arg
  if finish
    then then' arg
    else else' arg >>= tailRecM if' then' else'

fmapFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
fmapFst f = fmap (\(a, c) -> (f a, c))

tailRec2M :: (Monad m) => (a -> b -> m Bool) -> (a -> c) -> (b -> d) -> (a -> b -> m (a, b)) -> a -> b -> m (c, d)
tailRec2M if' thenA thenB else' a b = tailRecM (uncurry if') (\(a', b') -> return (thenA a', thenB b')) (uncurry else') (a, b)
