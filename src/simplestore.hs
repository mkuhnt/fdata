module SimpleStore
( Qualification(..)
, Store(..)
, Path
, create
, createIndexed
, dimensionality
, dimensions
, slice
, splice
, put
, get
, validQualification
) where

import qualified Data.Map as Map
import qualified Data.List as List

-- Qualifications define a selection on a dimension at a certain point.
-- The first argument picks the dimension by index and the second argument the value in the dimension
data Qualification = Qualification Int Int deriving (Eq, Ord, Show)

-- Store define the data structure
--  Store: First argument is the number of dimensions
--         Second argument is the index where the dimensions start
--         Third argument is the default value which is returned on not further specified data points
--         Fourth argument is the data structure that holds the embedded stores
data Store a = Store Int Int a (Map.Map Qualification (Store a)) | Point a deriving (Show)
type Path = [Qualification]

create :: Int -> a -> Store a
create n def = createIndexed n 1 def


createIndexed :: Int -> Int -> a -> Store a
createIndexed n idx def
  | n <= 0 = Point def
  | n >= 1 = Store n idx def Map.empty


dimensionality :: Store a -> Int
dimensionality (Point _) = 0
dimensionality (Store n _ _ _) = n


dimensions :: Store a -> [Int]
dimensions (Point _) = []
dimensions (Store n start _ _) = [start..(start + n - 1)]


slice :: Store a -> Qualification -> Store a
slice (Point _) _  = error "You cannot slice a point. It is atomic ;)"
slice (Store n i def m) q
  | n <= 0 = error "A store with zero dimensions is a point and cannot be sliced."
  | n == 1 = case Map.lookup q m of
    Nothing  -> Point def
    Just val -> val
  | n >= 2 = case Map.lookup q m of
    Nothing  -> createIndexed (n-1) (i+1) def
    Just val -> val


splice :: Store a -> Store a -> Qualification -> Store a
splice point@(Point v) (Store 1 i def m) q = Store 1 i def (Map.insert q point m)
splice store@(Store d1 i1 def1 m1) (Store d2 i2 def2 m2) q
  | d1 == (d2 - 1) = Store d2 i2 def2 (Map.insert q store m2)
  | otherwise      = error "You cannot splice these two stores."


put :: Store a -> Path -> a -> Store a
put (Point v) [] val = Point val
put store q val
  | (validQualification store path) = splice (put (slice store (head path)) (tail path) val) store (head path)
  | otherwise                       = error "The dimensionality of the path and the store don't match."
  where path = List.sort q


get :: Store a -> Path -> a
get (Point v) [] = v
get store q
  | (validQualification store path) = get (slice store (head path)) (tail path)
  | otherwise                       = error "The dimensionality of the path and the store don't match."
  where path = List.sort q


validQualification :: Store a -> Path -> Bool
validQualification (Point _) [] = True
validQualification s q = dimensions(s) == [a | (Qualification a b) <- q]
