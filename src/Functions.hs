module Functions (pair, (!?), (??), join, foldlMapM) where

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

join :: (Foldable f) => String -> f String -> String
join str xs
  | null xs = ""
  | otherwise = foldr1 (\a b -> a ++ str ++ b) xs

foldlMapM :: (Monad m, Foldable t) => (b -> a -> m (b, c)) -> b -> t a -> m (b, [c])
foldlMapM f seed = foldl combine $ return (seed, [])
    where combine accM item = accM >>= (\(acc, list) -> fmap (\(acc', item') -> (acc', item' : list)) (f acc item))
