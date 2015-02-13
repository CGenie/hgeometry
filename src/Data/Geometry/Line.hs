{-# Language
      TypeFamilies
  #-}
module Data.Geometry.Line where

import Prelude hiding(length)

import Data.Geometry.Point
import Data.Geometry.Geometry


import qualified Data.List  as L

---------------------------------------------------------------------
-- | A simple line segment in 2D consisint of a start and an end-point
data LineSegment2' a = LineSegment2 { startPoint :: Point2' a
                                    , endPoint   :: Point2' a
                                    }
                       deriving (Eq, Ord, Show, Read)

instance Functor LineSegment2' where
    fmap f (LineSegment2 p q) = LineSegment2 (fmap f p) (fmap f q)

instance IsPoint2Functor LineSegment2' where
    p2fmap f (LineSegment2 p q) = LineSegment2 (f p) (f q)

instance HasPoints LineSegment2' where
    points (LineSegment2 p q) = [p,q]

---------------------------------------------------------------------
-- | An infinite line

newtype Line2' a = Line2 (LineSegment2' a)
                 deriving (Eq,Ord,Show,Read)


instance Functor Line2' where
    fmap f (Line2 l) = Line2 $ fmap f l

instance IsPoint2Functor Line2' where
    p2fmap f (Line2 l) = Line2 $ p2fmap f l

instance HasPoints Line2' where
    points (Line2 l) = points l

---------------------------------------------------------------------
-- | Polylines
newtype Polyline2' a = Polyline2 [LineSegment2' a]
                   deriving (Eq, Show, Read)

instance IsPoint2Functor Polyline2' where
    p2fmap f (Polyline2 ls) = Polyline2 (map (p2fmap f) ls)

instance HasPoints Polyline2' where
    points (Polyline2 ls) = case ls of
                                 []      -> []
                                 (l:ls') -> points l ++ map endPoint ls'

---------------------------------------------------------------------
-- | Constructing polylines

polyLine :: [Point2' a] -> Polyline2' a
polyLine =  Polyline2 . makeLines
    where
      makeLines     :: [Point2' a] -> [LineSegment2' a]
      makeLines []  =
          error "Polyline consists of at least two points. No points given."
      makeLines [_] =
          error "Polyline consists of at least two points. Only one point given."
      makeLines pts = zipWith LineSegment2 pts (tail pts)

---------------------------------------------------------------------
-- | functions on Linesegments and Polylines

isSimpleLine                 :: Polyline2' a -> Bool
isSimpleLine (Polyline2 [])  = error "polyline without line segments"
isSimpleLine (Polyline2 [_]) = True
isSimpleLine _               = False

toSimpleLine                :: Polyline2' a -> LineSegment2' a
toSimpleLine (Polyline2 ls) = head ls

toSimpleLineOption    :: Polyline2' a -> Maybe (LineSegment2' a)
toSimpleLineOption p = if isSimpleLine p then Just (toSimpleLine p) else Nothing


---------------------------------------------------------------------
-- | Linear interpolation / points on line segments etc.

-- | simple linear interpolation, assuming t in [0,1]
linear       :: Num a => a -> a -> a -> a
linear t x y = (1-t)*x + t*y

inRange           :: Ord a => a -> (a,a) -> Bool
x `inRange` (a,b) = a <= x && x <= b

onLineSegment :: (Ord a, Fractional a) => Point2' a -> LineSegment2' a -> Bool
p `onLineSegment` l@(LineSegment2 s t) =
    if t == s then p == s else (lambda `inRange` (0,1) && p == pointAt lambda l)
    where
      a                  = p |-| s              -- the vector from s to p
      b                  = t |-| s              -- the vector from s to t
      lambda             = (a |@| b) / (len b)
      -- we translate such that s corresponds with the origin. In this coord system
      -- b represents the input line segment.
      -- We orthoganally project a onto b. Let c be this point (on the vector b)
      -- then : d = a |@| b / length b denotes the distance between (0,0) and c
      -- We can now get the lambda such that : c = linear (0,0) b  by dividing
      -- d / length b. Hence in total we divide through (length b)^2.  This means
      -- we can avoid computing the square root.
      len (Point2 (x,y)) = x^2 + y^2

class HasLength c where
    type PM c   -- the precision model
    -- | The length of the line-like segment
    length  :: c -> PM c

instance Floating a => HasLength (LineSegment2' a) where
    type PM (LineSegment2' a) = a
    length (LineSegment2 s t) = dist s t

instance Floating a => HasLength (Polyline2' a) where
    type PM (Polyline2' a) = a
    length (Polyline2 ls) = sum . map length $ ls


class LineLike c where
    -- | get the point at `time' t (t in [0,1])
    pointAt  :: Num a => a -> c a -> Point2' a

instance LineLike LineSegment2' where
    pointAt t (LineSegment2 (Point2 (px,py)) (Point2 (qx,qy))) =
        Point2 (linear t px qx, linear t py qy)
