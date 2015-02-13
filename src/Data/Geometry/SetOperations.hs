{-# Language
    MultiParamTypeClasses
  , FlexibleInstances
  , UndecidableInstances
  #-}
module Data.Geometry.SetOperations( AreIntersectable(..)
                                  ) where

import Data.List
import Data.Geometry.Point
import Data.Geometry.Line
import Data.Geometry.Circle

import Debug.Trace

--------------------------------------------------------------------------------

-- | A class to represent that a pair of geometry objects (both parameterized
-- over a) can be intersected.
class AreIntersectable g h a where
    intersectionPoints :: g a -> h a -> [Point2' a]

-- | Intersection is symetrical
-- instance AreIntersectable g h a => AreIntersectable h g a where
--     intersectionPoints h g = intersectionPoints g h


-- instance AreIntersectable LineSegment2' LineSegment2' a where
--     intersectionPoints _ _ = []

instance (Ord a, Floating a) => AreIntersectable Circle2' LineSegment2' a where
    -- | The intersection points, ordered along the line segment
    intersectionPoints c l = filter (`onLineSegment` l) $ circleAndLine c l

instance (Ord a, Floating a) => AreIntersectable Circle2' Polyline2' a where
    -- | The intersection points, ordered along the polyline
    intersectionPoints c (Polyline2 ls) = concatMap (intersectionPoints c) ls

instance (Ord a, Floating a) => AreIntersectable Circle2' Line2' a where
    intersectionPoints c (Line2 l) = circleAndLine c l


-- | represents the intersection of a circle and an infinite line (as LineSegment )
circleAndLine :: (Ord a, Floating a) => Circle2' a -> LineSegment2' a -> [Point2' a]
circleAndLine (Circle2 p r) (LineSegment2 s t) = map (|+| p) $ pts discr
        where
          s'@(Point2 (sx,sy))  = s  |-| p   -- translate so the circle is centred at (0,0)
          t'                   = t  |-| p
          d@(Point2 (dx,dy))   = t' |-| s'  -- vector from s' to t'
          ----
          -- any point on the line segment is given as: q = s' + \lambda * d
          q lamb               = s' |+| (lamb |*| d)
          -- solving the equation (q_x)^2 + (q_y)^2 = r^2 then yields the equation
          -- L^2(dx^2 + dy^2) + L2(sx*dx + sy*dy) + sx^2 + sy^2 = 0
          -- where L = \lambda
          a                    = dx^2 + dy^2
          b                    = 2*(sx*dx + sy*dy)
          c                    = sx^2 + sy^2 - r^2
          discr                = b^2 - 4*a*c
          discr'               = sqrt discr
          lambda (|+-|)        = (-b |+-| discr') / (2*a)
          pts dscr
              | dscr < 0       = []                -- no intersections
              | dscr == 0      = [q $ lambda (+)]  -- line tangent
              | otherwise      = let lambdas = sort [lambda (-), lambda (+)] in
                                 map q lambdas     -- intersection
