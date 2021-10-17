module MapChain (Maps, insert, empty, lookup) where

import qualified Data.Map as Map

insert :: k -> a -> [Map.Map k a] -> [Map.Map k a]
insert key value [] = [Map.singletion key value]
insert key value (x : xs) = Map.insert key value x : xs

empty :: [Map.Map k a]
empty = []

lookup :: k -> [Map.Map k a] -> Maybe a
lookup key (MapChain []) = Nothing
lookup key (MapChain (x : xs)) = case Map.lookup key x of
    Just x -> x
    Nothing -> lookup key (MapChain xs)