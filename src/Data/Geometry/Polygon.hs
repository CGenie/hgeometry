module Data.Geometry.Polygon where

import Data.Geometry.BoundingBox
import Data.Geometry.Point
import Data.Geometry.Geometry

---------------------------------------------------------------------
-- | Polygons

-- | Class that defines what a polygon is. Note that it is assumed that the
-- first and the last point are *NOT* the same point.
--
class (HasPoints p, IsTransformable p) => IsPolygon p where
    vertices :: p a -> [Point2' a]
    vertices = points

    -- | default implementation assumes points are in order
    edges   :: p a -> [(Point2' a, Point2' a)]
    edges p = let pts = points p in
              zip pts (tail pts ++ [head pts])

    isSimple      :: p a -> Bool
    containsHoles :: p a -> Bool

---------------------------------------------------------------------
-- | Simple polygons, i.e. polygons consisting of a sequence of points (vertices)
-- | such that the edges do not intersect. Simple polygons do not contain holes
data SimplePolygon' a = SimplePolygon [Point2' a]
                       deriving (Show,Eq)

instance HasPoints SimplePolygon' where
    points (SimplePolygon pts) = pts

instance IsPolygon SimplePolygon' where
    isSimple      = const  True
    containsHoles = const False

instance IsPoint2Functor SimplePolygon' where
    p2fmap f (SimplePolygon pts) = SimplePolygon (map f pts)

---------------------------------------------------------------------
-- | A multipolygon consists of several simple polygons
data MultiPolygon' a = MultiPolygon [SimplePolygon' a]
                      deriving (Show,Eq)

instance HasPoints MultiPolygon' where
    points (MultiPolygon pls) = concatMap points pls

instance IsPolygon MultiPolygon' where
    isSimple                    = const False
    containsHoles               = const False

instance IsPoint2Functor MultiPolygon' where
    p2fmap f (MultiPolygon polys) = MultiPolygon (map (p2fmap f) polys)

---------------------------------------------------------------------
-- | Bounding boxes can be used as polygons
instance IsPolygon BoundingBox2' where
    isSimple      = const True
    containsHoles = const False
