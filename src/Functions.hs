module Functions (zipMaps, pair, map2, (??), intercalate, join, pad, mapCatFoldlM, tailRecM, tailRec2M, fmap2, split, follow, leaf) where

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Tree as Tree

zipMaps :: Ord k => Map.Map k v -> Map.Map k w -> Map.Map k (v, w)
zipMaps = Merge.merge Merge.dropMissing Merge.dropMissing $ Merge.zipWithMatched $ const pair

pair :: a -> b -> (a, b)
pair a b = (a, b)

map2 :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
map2 f g (a, b) = (f a, g b)

fmap2 :: Functor f => (a -> c) -> (b -> d) -> f (a, b) -> f (c, d)
fmap2 f g = fmap (map2 f g)

infixr 2 ??
(??) :: Monad m => Maybe a -> m a -> m a
Nothing ?? m = m
Just x ?? _ = return x

intercalate :: (Foldable f, Monoid m) => m -> f m -> m
intercalate x xs
  | null xs = mempty
  | otherwise = foldr1 (\a b -> a <> x <> b) xs

join :: Show a => String -> [a] -> String
join str = intercalate str . map show

pad :: Integral i => i -> Char -> String -> String
pad n c s = replicate (fromIntegral n - length s) c ++ s

mapCatFoldlM :: (Monad m, Foldable t) => (b -> a -> m (b, [c])) -> b -> t a -> m (b, [c])
mapCatFoldlM f seed = fmap2 id (concat . reverse) . foldl combine (return (seed, []))
  where combine accM item = accM >>= (\(acc, list) -> fmap2 id (: list) $ f acc item)

tailRecM :: Monad m => (a -> m Bool) -> (a -> m b) -> (a -> m a) -> a -> m b
tailRecM if' then' else' arg = if' arg >>= action
  where action True = then' arg
        action False = else' arg >>= tailRecM if' then' else'

tailRec2M :: Monad m => (a -> b -> m Bool) -> (a -> c) -> (b -> d) -> (a -> b -> m (a, b)) -> a -> b -> m (c, d)
tailRec2M if' thenA thenB else' = curry $ tailRecM (uncurry if') (return . map2 thenA thenB) (uncurry else')

split :: [a] -> ([(a, a)], Maybe a)
split = splitTail []
  where splitTail acc [] = (acc, Nothing)
        splitTail acc [x] = (acc, Just x)
        splitTail acc (x1 : x2 : xs) = splitTail ((x1, x2) : acc) xs

follow :: Monad m => (a -> m (b, a)) -> (a -> m (c, a)) -> a -> m ((b, c), a)
follow f g x = do
  (a, y) <- f x
  (b, z) <- g y
  return ((a, b), z)

leaf :: a -> Tree.Tree a
leaf = (`Tree.Node` [])
