module Data.Geometry.Circle( Circle2'(..)
                           , Disc2'(..)
                           , IsCircleLike(..)
                           , inCircle
                           , insideCircle
                           , onCircle
                           , inDisc
                           , insideDisc
                           ) where

import Data.Geometry.Point
import Data.Geometry.Geometry

---------------------------------------------------------------------
-- | A circle in the plane

data Circle2' a = Circle2 (Point2' a) a
                deriving (Eq,Ord,Show,Read)

instance HasPoints Circle2' where
    points (Circle2 p _) = [p]

-- TODO: instance for transformable

---------------------------------------------------------------------
-- | A disc in the plane (i.e. a circle inclusiding its contents)

newtype Disc2' a = Disc2 { border :: Circle2' a }
                 deriving (Show,Eq,Ord,Read)


instance HasPoints Disc2' where
    points = points . border

-- TODO: instance for transformable

--------------------------------------------------------------------------------
-- | functions on circles

-- | Class expressing functions that circlelike objects all have. Like a center
-- and a radius. Minimal implementation is either getCircle or center and radius
class IsCircleLike t where
    getCircle   :: t a -> Circle2' a
    getCircle x = Circle2 (center x) (radius x)

    center :: t a -> Point2' a
    center = center . getCircle

    radius :: t a -> a
    radius = radius . getCircle

    distance   :: Floating a => Point2' a -> t a -> a
    distance p = distance p . getCircle

    distanceToCenter   :: Floating a => Point2' a -> t a -> a
    distanceToCenter p = distanceToCenter p . getCircle

instance IsCircleLike Circle2' where
    center (Circle2 p _)       = p
    radius (Circle2 _ r)       = r
    distanceToCenter p (Circle2 q _) = dist p q

    distance p c@(Circle2 _ r) = distanceToCenter p c - r

instance IsCircleLike Disc2' where
    getCircle = border

--------------------------------------------------------------------------------
-- | Checking if points lie in or on a circle/disc


-- | Squared distance to the center
l22ToCenter :: Num a => Point2' a -> Circle2' a -> a
l22ToCenter p (Circle2 q _) = l22dist p q

-- | whether or not p lies in OR on the circle c
inCircle       :: (Ord a, Num a) => Point2' a -> Circle2' a -> Bool
p `inCircle` c = l22ToCenter p c <= (radius c)^2

-- | whether or not p lies strictly inside the circle c
insideCircle       :: (Num a, Ord a) => Point2' a -> Circle2' a -> Bool
p `insideCircle` c = l22ToCenter p c < (radius c)^2

-- | whether or not p lies on the circle
onCircle                          :: (Eq a, Num a) => Point2' a -> Circle2' a -> Bool
p `onCircle` c = l22ToCenter p c == (radius c)^2


-- | whether or not a point lies in a disc: this includes its border
inDisc               :: (Num a, Ord a) => Point2' a -> Disc2' a -> Bool
p `inDisc` (Disc2 c) = p `inCircle` c

-- | whether or not a point lies strictly inside a disc.
insideDisc :: (Num a, Ord a) => Point2' a -> Disc2' a -> Bool
p `insideDisc` (Disc2 c) = p `insideCircle` c
