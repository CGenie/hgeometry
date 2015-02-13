module Data.Geometry.Point where


data Point2' a = Point2 (a,a)
               deriving (Show,Read,Eq,Ord)


instance Functor Point2' where
    fmap f (Point2 (x,y)) = Point2 (f x,f y)

(|+|)                             :: Num a => Point2' a -> Point2' a -> Point2' a
(Point2 (x,y)) |+| (Point2 (a,b)) =  Point2 (x+a,y+b)

(|-|)                             :: Num a => Point2' a -> Point2' a -> Point2' a
(Point2 (x,y)) |-| (Point2 (a,b)) = Point2 (x-a,y-b)


-- | scalar multiplication
(|*|)                :: Num a => a -> Point2' a -> Point2' a
s |*| (Point2 (x,y)) = Point2 (s*x,s*y)


-- | dot product
(|@|) :: Num a => Point2' a -> Point2' a -> a
(Point2 (x,y)) |@| (Point2 (a,b)) = x*a + y*b


getX                :: Point2' a -> a
getX (Point2 (x,_)) = x

getY                :: Point2' a -> a
getY (Point2 (_,y)) = y

-- | euclidean distance between p and q
dist     :: Floating a => Point2' a -> Point2' a -> a
dist p q = sqrt $ l22dist p q

-- | Squared euclidean distance between p and q
l22dist :: Num a => Point2' a -> Point2' a -> a
l22dist p q = let (Point2 (a,b)) = q |-| p in  a*a + b*b
