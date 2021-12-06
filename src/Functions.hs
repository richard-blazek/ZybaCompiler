module Functions where

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

fill :: b -> [a] -> [b]
fill = map . const

join :: Foldable t => [a] -> t [a] -> [a]
join element xs = if null xs then [] else foldr1 (\a b -> a ++ element ++ b) xs

unionMapWith :: (Foldable t, Ord k) => Map.Map k a -> t (k, a) -> (Map.Map k a, [(k, a, a)])
unionMapWith map foldable = foldl combine (map, []) foldable
    where combine (map, duplicates) (key, value) = case Map.lookup key map of
            Just previous -> (map, (key, value, previous) : duplicates)
            Nothing -> (Map.insert key value map, duplicates)