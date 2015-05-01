import qualified Data.Map as Map

data Qualification = Qualification Int Int deriving (Eq, Ord, Show)
data Store = Store Int (Map.Map Qualification Store) | Point Int deriving (Show)


create :: Int -> Store
create n
  | n <= 0 = Point 0
  | n >= 1 = Store n Map.empty


dimensionality :: Store -> Int
dimensionality (Point _) = 0
dimensionality (Store n _) = n


slice :: Store -> Qualification -> Store
slice (Point _) _  = error "You cannot slice a point. It is atomic ;)"
slice (Store n m) q
  | n <= 0 = error "A store with zero dimensions is a point and cannot be sliced."
  | n == 1 = case Map.lookup q m of
    Nothing  -> Point 0
    Just val -> val
  | n >= 2 = case Map.lookup q m of
    Nothing  -> create (n-1)
    Just val -> val


splice :: Store -> Store -> Qualification -> Store
splice (Point v) (Store 1 m) q = Store 1 (Map.insert q (Point v) m)
splice (Store d1 m1) (Store d2 m2) q
  | d1 == (d2 - 1) = Store d2 (Map.insert q (Store d1 m1) m2)
  | otherwise      = error "You cannot splice these two stores."


put :: Store -> [Qualification] -> Int -> Store
put (Point v) [] val = Point val
put store q val
  | (dimensionality store) == (length q) = splice (put (slice store (head q)) (tail q) val) store (head q)
  | otherwise                            = error "The dimensionality of the path and the store don't match."


get :: Store -> [Qualification] -> Int
get (Point v) [] = v
get store q
  | (dimensionality store) == (length q) = get (slice store (head q)) (tail q)
  | otherwise                            = error "The dimensionality of the path and the store don't match."
