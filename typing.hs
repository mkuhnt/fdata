import qualified Data.Map as Map

data Store = Store Int (Map.Map Int Int) | Point Int deriving (Show)
data Qualification = Qualification Int Int deriving (Show)

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
  | n == 1 = Point 0
  | n >= 2 = Store (n-1) m

splice :: Store -> Store -> Qualification -> Store
splice (Point _) (Store 1 m) (Qualification _ _) = Store 1 m
splice (Store d1 m1) (Store d2 m2) (Qualification _ _)
  | d1 == (d2 - 1) = Store d2 m2
  | otherwise      = error "You cannot splice these two stores."

put :: Store -> [Qualification] -> Int -> Store
put (Point v) [] val = Point val
put store q val
  | (dimensionality store) == (length q) = splice (put (slice store (head q)) (tail q) val) store (head q)
  | otherwise                            = error "The dimensionality of the path and the store don't match."
