module Functions (pair, (!?), (??), intercalate, join, foldlMapM, tailRecM, tailRec2M, fmapFst, split, follow) where

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

intercalate :: (Foldable f, Monoid m) => m -> f m -> m
intercalate x xs
  | null xs = mempty
  | otherwise = foldr1 (\a b -> a <> x <> b) xs

join :: (Show a) => String -> [a] -> String
join str xs = intercalate str (map show xs)

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

split :: [a] -> Maybe [(a, a)]
split [] = Just []
split [x] = Nothing
split (x1 : x2 : xs) = fmap ((x1, x2) :) $ split xs

follow :: Monad m => (a -> m (b, a)) -> (a -> m (c, a)) -> a -> m ((b, c), a)
follow f g x = do
  (a, y) <- f x
  (b, z) <- g y
  return ((a, b), z)
