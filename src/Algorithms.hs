module Algorithms (distinctSort, mergeSort) where

merge :: (Ord t) => [t] -> [t] -> [t]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x > y = y : mergeDistinct (x : xs) ys
    | otherwise = x : mergeDistinct xs (y : ys)

mergeDistinct :: (Ord t) => [t] -> [t] -> [t]
mergeDistinct xs [] = xs
mergeDistinct [] ys = ys
mergeDistinct (x:xs) (y:ys)
    | x < y = x : mergeDistinct xs (y : ys)
    | x > y = y : mergeDistinct (x : xs) ys
    | otherwise = x : mergeDistinct xs ys


listify :: t -> [t]
listify x = [x]

mergeLevel :: (Ord t) => ([t] -> [t] -> [t]) -> [[t]] -> [[t]]
mergeLevel merger (first : (second : rest)) = merger first second : mergeLevel merger rest
mergeLevel _ lol = lol

mergeAll :: (Ord t) => ([t] -> [t] -> [t]) -> [[t]] -> [t]
mergeAll _ [] = []
mergeAll _ (x:[]) = x
mergeAll merger lol = mergeAll merger (mergeLevel merger lol)

sortWith :: (Ord t) => ([t] -> [t] -> [t]) -> [t] -> [t]
sortWith merger = (mergeAll merger) . (map listify)

mergeSort :: (Ord t) => [t] -> [t]
mergeSort = sortWith merge

distinctSort :: (Ord t) => [t] -> [t]
distinctSort = sortWith mergeDistinct
