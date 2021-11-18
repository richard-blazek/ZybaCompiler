module Functions where

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