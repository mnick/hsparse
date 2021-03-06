{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Sparse.Vector where

import qualified Data.IntMap             as M hiding ((!))
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           Numeric.Sparse.Internal
import           Numeric.Sparse.Types

instance (Eq a, Num a, DIM n) => Num (SparseVector n a) where
  (+)           = unionWith (+)
  (*)           = intersectWith (*)
  negate        = fmap negate
  fromInteger 0 = empty
  fromInteger x = singleton 1 (fromInteger x)
  abs           = fmap abs
  signum        = fmap signum

null :: (DIM n) => SparseVector n a -> Bool
null (SV v) = M.null v

-- | Dot product of two `IntMap`s (for internal use)
spDot :: (Num a, Eq a, DIM n) => SparseVector n a -> SparseVector n a -> Maybe a
spDot (SV v) (SV w) = case M.foldl' (+) 0 $ M.intersectionWith (*) v w of
  0 -> Nothing
  x -> Just x

checkDim :: forall n a. (DIM n) => Index -> SparseVector n a -> SparseVector n a
checkDim i v | i < 1 || i > d = error $ "Index out of bounds (" ++ show i ++ " not in (1," ++ show d ++ "))"
             | otherwise = v
   where
     d = natInt (Proxy :: Proxy n)

checkDim' :: Index -> (Index, a) -> (Index, a)
checkDim' d (i, v) | i < 1 || i > d = error $ "Index out of bounds (" ++ show i ++ " not in (1," ++ show d ++ "))"
                   | otherwise = (i, v)

empty :: forall a n. (DIM n, Num a) => SparseVector n a
empty = SV M.empty

nnz :: (DIM n) => SparseVector n a -> Int
nnz (SV v) = M.size v

singleton :: (DIM n) => Index -> a -> SparseVector n a
singleton i x = SV (M.singleton i x)

unionWith :: (Eq a, Num a, DIM n) => (a -> a -> a) -> SparseVector n a -> SparseVector n a -> SparseVector n a
unionWith f (SV x) (SV y) = SV $ M.filter (/= 0) (M.unionWith f x y)

intersectWith :: DIM n => (a -> a -> a) -> SparseVector n a -> SparseVector n a -> SparseVector n a
intersectWith f (SV x) (SV y) = SV $ M.intersectionWith f x y

append :: forall n m a. (DIM n, KnownNat m) => SparseVector n a -> SparseVector m a -> SparseVector (n + m) a
append (SV x) (SV y) = SV (M.union x (shiftKeys s y))
  where
    s = natInt (Proxy :: Proxy n)

-- Serialization --------------------------------------------------------------

toListWithSize :: forall n a. (Num a, Eq a, DIM n) => SparseVector n a -> (Int, [(Index, a)])
toListWithSize (SV v) = (natInt (Proxy :: Proxy n), M.toList v)

toList :: (Num a, Eq a, DIM n) => SparseVector n a -> [(Index, a)]
toList = snd . toListWithSize

fromList :: forall n a. (Num a, Eq a, DIM n) => [(Index, a)] -> SparseVector n a
fromList ivs = SV (M.fromList $ map (checkDim' d) ivs)
   where
     d = natInt (Proxy :: Proxy n)

-- Modification ---------------------------------------------------------------

del :: (Num a, DIM n) => SparseVector n a -> Index -> SparseVector n a
del v i = SV (M.delete i (vec $ checkDim i v))

del' :: (Num a, DIM n) => SparseVector n a -> Index -> Maybe (SparseVector n a)
del' v i = case M.size v' of
                  0 -> Nothing
                  otherwise -> Just (SV v')
  where
    v' = M.delete i $ vec (checkDim i v)

ins :: (Eq a, Num a, DIM n) => SparseVector n a -> (Index, a) -> SparseVector n a
ins v (i, 0) = del (checkDim i v) i
ins v (i, x) = SV (M.insert i x $ vec (checkDim i v))

-- Linear Algebra -------------------------------------------------------------

-- | Scale sparse vector by a scalar a * v
scale :: forall n a. (Eq a, Num a, DIM n) => a -> SparseVector n a -> SparseVector n a
scale 0 v = empty
scale 1 v = v
scale c v = fmap (* c) v

-- | Inner (dot) product of two sparse vectors <x,y>
dot :: (Eq a, Num a, DIM n) => SparseVector n a -> SparseVector n a -> a
dot x y = fromMaybe 0 (spDot x y)

(<>) :: (Eq a, Num a, DIM n) => SparseVector n a -> SparseVector n a -> a
x <> y = dot x y

-- | l2 norm of vector
norm :: (Eq a, Num a, Floating a, DIM n) => SparseVector n a -> a
norm v = sqrt $ sum $ fmap (^ (2::Int)) v

-- | Outer product
outer :: (Eq a, Num a, DIM n, DIM m) => SparseVector n a -> SparseVector m a -> SparseMatrix n m a
outer (SV x) v = SM $ M.map (`scale` v) x

(><) :: (Eq a, Num a, DIM n, DIM m) => SparseVector n a -> SparseVector m a -> SparseMatrix n m a
x >< y = outer x y
