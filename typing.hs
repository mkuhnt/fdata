import qualified Data.Map as Map
import qualified Data.List as List

data Qualification = Qualification Int Int deriving (Eq, Ord, Show)
data Store = Store Int Int (Map.Map Qualification Store) | Point Int deriving (Show)


create :: Int -> Store
create n = createIndexed n 1


createIndexed :: Int -> Int -> Store
createIndexed n idx
  | n <= 0 = Point 0
  | n >= 1 = Store n idx Map.empty


dimensionality :: Store -> Int
dimensionality (Point _) = 0
dimensionality (Store n _ _) = n


dimensions :: Store -> [Int]
dimensions (Point _) = []
dimensions (Store n start _) = [start..(start + n - 1)]


slice :: Store -> Qualification -> Store
slice (Point _) _  = error "You cannot slice a point. It is atomic ;)"
slice (Store n i m) q
  | n <= 0 = error "A store with zero dimensions is a point and cannot be sliced."
  | n == 1 = case Map.lookup q m of
    Nothing  -> Point 0
    Just val -> val
  | n >= 2 = case Map.lookup q m of
    Nothing  -> createIndexed (n-1) (i+1)
    Just val -> val


splice :: Store -> Store -> Qualification -> Store
splice (Point v) (Store 1 i m) q = Store 1 i (Map.insert q (Point v) m)
splice (Store d1 i1 m1) (Store d2 i2 m2) q
  | d1 == (d2 - 1) = Store d2 i2 (Map.insert q (Store d1 i1 m1) m2)
  | otherwise      = error "You cannot splice these two stores."


put :: Store -> [Qualification] -> Int -> Store
put (Point v) [] val = Point val
put store q val
  | (validQualification store path) = splice (put (slice store (head path)) (tail path) val) store (head path)
  | otherwise                       = error "The dimensionality of the path and the store don't match."
  where path = List.sort q


get :: Store -> [Qualification] -> Int
get (Point v) [] = v
get store q
  | (validQualification store path) = get (slice store (head path)) (tail path)
  | otherwise                       = error "The dimensionality of the path and the store don't match."
  where path = List.sort q


validQualification :: Store -> [Qualification] -> Bool
validQualification (Point _) [] = True
validQualification s q = dimensions(s) == [a | (Qualification a b) <- q]
