module Store
( Qualification(..)
, Store
, createStore
, dimensions
, dimensionRange
, fullyQualified
, put
, get
) where

import qualified Data.Map as Map
import qualified Data.List as List

data Qualification a = Qualification a Int deriving (Eq, Ord, Show)
type Path a = [Qualification a]

data Store a b = Store [a] b (Map.Map (Path a) b) deriving (Show)

createStore :: [a] -> b -> Store a b
createStore rng dflt = Store rng dflt Map.empty

dimensions :: Store a b -> Int
dimensions (Store r _ _) = (length r)

dimensionRange :: Store a b -> [a]
dimensionRange (Store r _ _) = r

fullyQualified :: (Ord a) => Store a b -> Path a -> Bool
fullyQualified (Store r d m) p = (List.sort r) == [x | (Qualification x y) <- (canonize p)]

canonize :: (Ord a) => Path a -> Path a
canonize path = List.sort path

put :: (Ord a) => Store a b -> Path a -> b -> Store a b
put store@(Store rng dflt mp) path value
  | (fullyQualified store path) = (Store rng dflt (Map.insert (canonize path) value mp))
  | otherwise                   = error "error cannot store value without fully qualified path"

get :: (Ord a) => Store a b -> Path a -> b
get store@(Store rng dflt mp) path
  | (fullyQualified store path) = case Map.lookup path mp of
    Nothing    -> dflt
    Just value -> value
