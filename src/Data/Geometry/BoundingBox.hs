{-# Language
             FlexibleInstances,
             UndecidableInstances
 #-}
module Data.Geometry.BoundingBox(
                                -- ** BoundingBoxes
                                  BoundingBox2'(..)
                                , IsBoxable(..)
                                , mergeBoxes
                                , bbFromPoints
                                , bbLeft
                                , bbRight
                                , bbTop
                                , bbBottom
                                , width
                                , height
                                ) where


import Data.Geometry.Point
import Data.Geometry.Geometry

---------------------------------------------------------------------
-- | Bounding boxes

-- | Note that a bounding box is always axis parallel, so rotating may have not
-- | the expected effect


data BoundingBox2' a = BoundingBox2 { lowerLeft   :: Point2' a
                                    , upperRight  :: Point2' a
                                    }
                     deriving (Show,Eq,Read)


instance IsPoint2Functor BoundingBox2'  where
    p2fmap f (BoundingBox2 p q) = BoundingBox2 (f p) (f q)

instance HasPoints BoundingBox2' where
    points (BoundingBox2 p@(Point2 (x,y)) q@(Point2 (a,b))) =
        [p,Point2 (x,b), q, Point2 (a,y)]

---------------------------------------------------------------------
-- |

-- | A class of objects for which we can compute a boundingbox
class IsBoxable g where
    boundingBox        :: Ord a => g a -> BoundingBox2' a

    -- TODO: it would be nice if we can use some similar trick as used in
    -- show, to get an instance of IsBoxable for things of type [g a]
    bbFromList    :: Ord a => [g a] -> BoundingBox2' a
    bbFromList = mergeBoxes . map boundingBox


bbFromPoints     :: Ord a => [Point2' a] -> BoundingBox2' a
bbFromPoints pts = BoundingBox2 (Point2 (llx,lly)) (Point2 (urx,ury))
                      where
                        xs  = map getX pts
                        ys  = map getY pts
                        llx = minimum xs
                        lly = minimum ys
                        urx = maximum xs
                        ury = maximum ys

-- | get the bounding box of a list of things
mergeBoxes :: Ord a => [BoundingBox2' a] -> BoundingBox2' a
mergeBoxes = bbFromPoints . concatMap points

instance HasPoints p => IsBoxable p where
    boundingBox = bbFromPoints . points


width   :: Num a => BoundingBox2' a -> a
width b = bbRight b - bbLeft b

height   :: Num a => BoundingBox2' a -> a
height b = bbTop b - bbBottom b

bbLeft :: BoundingBox2' a -> a
bbLeft = getX . lowerLeft

bbRight :: BoundingBox2' a -> a
bbRight = getX . upperRight

bbTop :: BoundingBox2' a -> a
bbTop  = getY . upperRight

bbBottom :: BoundingBox2' a -> a
bbBottom = getY . lowerLeft
