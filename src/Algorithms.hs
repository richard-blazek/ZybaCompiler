module Algorithms (distinctSort, mergeSort) where

merge :: (Ord t) => [t] -> [t] -> [t] -> [t]
merge merged xs [] = xs ++ merged
merge merged [] ys = ys ++ merged
merge merged (x:xs) (y:ys)
    | x > y = merge (y : merged) (x : xs) ys
    | otherwise = merge (x : merged) xs (y : ys)

mergeDistinct :: (Ord t) => [t] -> [t] -> [t] -> [t]
mergeDistinct merged xs [] = xs ++ merged
mergeDistinct merged [] ys = ys ++ merged
mergeDistinct merged (x:xs) (y:ys)
    | x < y = mergeDistinct (x : merged) xs (y : ys)
    | x > y = mergeDistinct (y : merged) (x : xs) ys
    | otherwise = mergeDistinct (x : merged) xs ys


listify :: t -> [t]
listify x = [x]

mergeLevel :: (Ord t) => ([t] -> [t] -> [t]) -> [[t]] -> [[t]] -> [[t]]
mergeLevel merger merged [] = merged
mergeLevel merger merged (first : []) = first : merged
mergeLevel merger merged (first : (second : rest)) = mergeLevel merger (merger first second : merged) rest

mergeAll :: (Ord t) => ([t] -> [t] -> [t]) -> [[t]] -> [t]
mergeAll _ [] = []
mergeAll _ (x:[]) = x
mergeAll merger lol = mergeAll merger (mergeLevel merger [] lol)

sortWith :: (Ord t) => ([t] -> [t] -> [t] -> [t]) -> [t] -> [t]
sortWith merger = (mergeAll (merger [])) . (map listify)

mergeSort :: (Ord t) => [t] -> [t]
mergeSort = sortWith merge

distinctSort :: (Ord t) => [t] -> [t]
distinctSort = sortWith mergeDistinct
