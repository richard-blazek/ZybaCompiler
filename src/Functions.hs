module Functions (pair, apply, (!?), (??), fill) where

pair :: a -> b -> (a, b)
pair a b = (a, b)

apply :: (a -> b -> t) -> ((a, b) -> t)
apply fn (a, b) = fn a b

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
