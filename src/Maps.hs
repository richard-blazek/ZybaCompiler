module Maps (insert, Maps.lookup) where

import qualified Data.Map as Map

insert :: Ord k => k -> a -> [Map.Map k a] -> [Map.Map k a]
insert key value [] = [Map.singleton key value]
insert key value (x : xs) = Map.insert key value x : xs

lookup :: Ord k => k -> [Map.Map k a] -> Maybe a
lookup key [] = Nothing
lookup key (x : xs) = case Map.lookup key x of
    Nothing -> Maps.lookup key xs
    just -> just