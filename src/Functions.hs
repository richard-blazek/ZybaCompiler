module Functions (pair, pairM, (!?), (??), fill, join, tryInsert, foldlMapM) where

import qualified Data.Map.Strict as Map

pair :: a -> b -> (a, b)
pair a b = (a, b)

pairM :: Monad m => m a -> m b -> m (a, b)
pairM m = (>>= (\a -> fmap (`pair` a) m))

infixl 9 !?
(!?) :: Integral i => [a] -> i -> Maybe a
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)
_ !? _ = Nothing

infixl 3 ??
(??) :: Maybe a -> a -> a
Nothing ?? x = x
Just x ?? _ = x

fill :: b -> [a] -> [b]
fill = map . const

join :: (Show s, Show h) => s -> [h] -> String
join _ [] = ""
join element xs = let str = show element in foldr1 (\a b -> a ++ str ++ b) (map show xs)

foldlMapM :: (Monad m, Foldable t) => (b -> a -> m (b, c)) -> b -> t a -> m (b, [c])
foldlMapM f seed = foldl combine $ return (seed, [])
    where combine accM item = accM >>= (\(acc, list) -> fmap (\(acc', item') -> (acc', item' : list)) (f acc item))

tryInsert :: (Monad m, Ord k) => m (Map.Map k a) -> k -> a -> Map.Map k a -> m (Map.Map k a)
tryInsert err k v m
    | Map.member k m = err
    | otherwise = return $ Map.insert k v m