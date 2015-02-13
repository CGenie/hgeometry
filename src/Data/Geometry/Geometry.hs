{-# Language FlexibleInstances,
             UndecidableInstances,
             OverlappingInstances
  #-}
module Data.Geometry.Geometry(
                              -- ** Point based geometries
                               IsPoint2Functor(..)
                             , IsTransformable(..)
                             , HasPoints(..)
                             , Vec3(..)
                             , Matrix3(..)
                             , identityMatrix3
                             , matrix3FromLists
                             , matrix3FromList
                             , matrix3ToList
                             , matrix3ToLists
                             ) where


import Data.Geometry.Point

---------------------------------------------------------------------
-- | Point based geometries

-- | A class that defines a point2 functor. This defines that every operation that
-- we can do on a point we can also do on instances of this class. i.e. by
-- applying the operation on the underlying points.

class IsPoint2Functor g where
    p2fmap :: (Point2' a -> Point2' b) -> g a -> g b

class HasPoints g where
    points :: g a -> [Point2' a]

instance HasPoints Point2' where
    points p = [p]

---------------------------------------------------------------------
-- | Basic linear algebra to support affine transformations in 2D

-- | Type to represent a matrix, form is:
-- [ [ a11, a12, a13 ]
--   [ a21, a22, a23 ]
--   [ a31, a32, a33 ] ]

newtype Vec3    a = Vec3 (a,a,a)
                   deriving (Show,Eq)
newtype Matrix3 a = Matrix3 (Vec3 (Vec3 a))
                   deriving (Show,Eq)

instance Functor Vec3 where
    fmap f (Vec3 (a,b,c)) = Vec3 (f a, f b, f c)

instance Functor Matrix3 where
    fmap f (Matrix3 (Vec3 (a,b,c))) = Matrix3 $ Vec3 (fmap f a, fmap f b, fmap f c)

v3FromList         :: [a] -> Vec3 a
v3FromList [a,b,c] = Vec3 (a,b,c)
v3FromList _       = error "v3FromList needs exactly 3 elements."


-- | given a single list of 9 elements, construct a Matrix3
matrix3FromList :: [a] -> Matrix3 a
matrix3FromList = matrix3FromLists . rtake 3
                  where
                    rtake k ss = let (xs,ys) = splitAt k ss in
                                 xs : rtake k ys

-- | Given a 3x3 matrix as a list of lists, convert it to a Matrix3
matrix3FromLists :: [[a]] -> Matrix3 a
matrix3FromLists = Matrix3 . v3FromList . map v3FromList

v3ToList                :: Vec3 a -> [a]
v3ToList (Vec3 (a,b,c)) = [a,b,c]


-- | Gather the elements of the matrix in one long list (in row by row order)
matrix3ToList :: Matrix3 a -> [a]
matrix3ToList = concat . matrix3ToLists

matrix3ToLists                          :: Matrix3 a -> [[a]]
matrix3ToLists (Matrix3 (Vec3 (a,b,c))) = map v3ToList [a,b,c]


identityMatrix3 :: Num a => Matrix3 a
identityMatrix3 = matrix3FromLists [ [ 1, 0, 0 ]
                                   , [ 0, 1, 0 ]
                                   , [ 0, 0, 1 ] ]

multiply3 :: Num a => Matrix3 a -> Vec3 a -> Vec3 a
multiply3 (Matrix3 (Vec3 ( Vec3 (a,b,c)
                         , Vec3 (d,e,f)
                         , Vec3 (g,h,i)))) (Vec3 (x,y,z)) =
    Vec3 ( a*x + b*y + c*z,
           d*x + e*y + f*z,
           g*z + h*y + i*z)





-- | Class that indicates that something can be transformable using
-- an affine transformation

class IsTransformable g where
    transformWith :: Num a => Matrix3 a -> g a -> g a

-- | Points are transformable
instance IsTransformable Point2' where
    transformWith = transformPoint

transformPoint                  :: Num a => Matrix3 a -> Point2' a -> Point2' a
transformPoint m (Point2 (x,y)) =  Point2 (x',y')
    where
      v              = Vec3 (x,y,1)
      Vec3 (x',y',_) = multiply3 m v

-- | Everything that is built from points is transformable
tranformPointBased   :: (Num a, IsPoint2Functor g) => Matrix3 a -> g a -> g a
tranformPointBased m = p2fmap (transformWith m)

instance IsPoint2Functor g => IsTransformable g where
    transformWith = tranformPointBased
