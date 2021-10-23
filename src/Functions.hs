module Functions (distinctSort, mergeSort, pair, apply, (!?), (??)) where

merge :: (Ord t) => [t] -> [t] -> [t] -> [t]
merge result [] [] = reverse result
merge result (x : xs) [] = merge (x : result) xs []
merge result [] (y : ys) = merge (y : result) [] ys
merge result (x : xs) (y : ys)
    | x > y = merge (y : result) (x : xs) ys
    | otherwise = merge (x : result) xs (y : ys)

mergeDistinct :: (Ord t) => [t] -> [t] -> [t] -> [t]
mergeDistinct result [] [] = reverse result
mergeDistinct result [] ys = mergeDistinct result ys []
mergeDistinct [] (x : xs) [] = mergeDistinct [x] xs []
mergeDistinct (r : rs) (x : xs) [] = mergeDistinct (if r == x then r : rs else x : r : rs) xs []
mergeDistinct result (x : xs) (y : ys)
    | x > y = mergeDistinct (y : result) (x : xs) ys
    | x < y = mergeDistinct (x : result) xs (y : ys)
    | otherwise = mergeDistinct (x : result) xs ys

listify :: t -> [t]
listify x = [x]

mergeLevel :: (Ord t) => ([t] -> [t] -> [t]) -> [[t]] -> [[t]] -> [[t]]
mergeLevel merger merged [] = merged
mergeLevel merger merged [first] = first : merged
mergeLevel merger merged (first : (second : rest)) = mergeLevel merger (merger first second : merged) rest

mergeAll :: (Ord t) => ([t] -> [t] -> [t]) -> [[t]] -> [t]
mergeAll merger [] = []
mergeAll merger [x] = x
mergeAll merger lol = mergeAll merger (mergeLevel merger [] lol)

sortWith :: (Ord t) => ([t] -> [t] -> [t] -> [t]) -> [t] -> [t]
sortWith merger = mergeAll (merger []) . map listify

mergeSort :: (Ord t) => [t] -> [t]
mergeSort = sortWith merge

distinctSort :: (Ord t) => [t] -> [t]
distinctSort = sortWith mergeDistinct

pair :: a -> b -> (a, b)
pair a b = (a, b)

apply :: (a, b) -> (a -> b -> t) -> t
apply (a, b) fn = fn a b

infixl 9 !?
(!?) :: Integral i => [a] -> i -> Maybe a
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)
_ !? _ = Nothing

infixl 3 ??
(??) :: Maybe a -> a -> a
Nothing ?? x = x
Just x ?? _ = x
