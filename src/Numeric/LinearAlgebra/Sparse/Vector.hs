{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.LinearAlgebra.Sparse.Vector where

import           Data.Foldable as F
import           Data.IntMap   (IntMap)
import qualified Data.IntMap   as M hiding ((!))
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import Numeric.LinearAlgebra.Sparse.Internal


data SparseVector (n :: Nat) a = SV {vec :: !(IntMap a)}
                                  deriving (Eq, Read)

instance (DIM n) => Functor (SparseVector n) where
  fmap f v = v {vec = fmap f (vec v)}

instance (DIM n) => Foldable (SparseVector n) where
  foldr f d v = F.foldr f d (vec v)

instance (Eq a, Num a, DIM n) => Num (SparseVector n a) where
  (+)           = unionWith (+)
  (*)           = intersectWith (*)
  negate        = fmap negate
  fromInteger 0 = empty
  fromInteger x = singleton (fromInteger x)
  abs           = fmap abs
  signum        = fmap signum

-- instance (DIM n, Num a) => Monoid (SparseVector n a) where
--   mempty = empty
--   mappend = append

instance (Show a, Eq a, Num a, DIM n) => Show (SparseVector n a) where
  show (SV v) = show (natInt (Proxy :: Proxy n)) ++ ": " ++ show v

-- | Dot product of two `IntMap`s (for internal use)
spDot :: (Num a, Eq a, DIM n) => SparseVector n a -> SparseVector n a -> Maybe a
spDot (SV v) (SV w) = case M.foldl' (+) 0 $ M.intersectionWith (*) v w of
  0 -> Nothing
  x -> Just x

empty :: forall a n. (DIM n, Num a) => SparseVector n a
empty = SV M.empty

nnz :: (DIM n) => SparseVector n a -> Int
nnz (SV v) = M.size v

singleton :: (DIM n) => a -> SparseVector n a
singleton x = SV (M.singleton 1 x)

unionWith :: (Eq a, Num a, DIM n) => (a -> a -> a) -> SparseVector n a -> SparseVector n a -> SparseVector n a
unionWith f (SV x) (SV y) = SV $ M.filter (/= 0) (M.unionWith f x y)

intersectWith :: DIM n => (a -> a -> a) -> SparseVector n a -> SparseVector n a -> SparseVector n a
intersectWith f (SV x) (SV y) = SV $ M.intersectionWith f x y

append :: forall n m a. (DIM n, KnownNat m) => SparseVector n a -> SparseVector m a -> SparseVector (n + m) a
append (SV x) (SV y) = SV (M.union x (shiftKeys s y))
  where
    s = natInt (Proxy :: Proxy n)

toAssocListWithSize :: forall n a. (Num a, Eq a, DIM n) => SparseVector n a -> (Int, [(Index, a)])
toAssocListWithSize (SV v) = (natInt (Proxy :: Proxy n), M.toAscList v)

toAssocList :: forall n a. (Num a, Eq a, DIM n) => SparseVector n a -> [(Index, a)]
toAssocList = snd . toAssocListWithSize

fromAssocList :: (Num a, Eq a, DIM n) => [(Index, a)] -> SparseVector n a
fromAssocList l = SV (M.fromAscList l)

-- Modifications
del :: (Num a, DIM n) => SparseVector n a -> Index -> SparseVector n a
del (SV v) i = SV (M.delete i v)

del' :: (Num a, DIM n) => SparseVector n a -> Index -> Maybe (SparseVector n a)
del' (SV v) i = case M.size v' of
                  0 -> Nothing
                  otherwise -> Just (SV v')
  where
    v' = M.delete i v

ins :: (Eq a, Num a, DIM n) => SparseVector n a -> (Index, a) -> SparseVector n a
ins v (i, 0) = del v i
ins (SV v) (i, x) = SV (M.insert i x v)

dot :: (Eq a, Num a, DIM n) => SparseVector n a -> SparseVector n a -> a
dot x y = fromMaybe 0 (spDot x y)

(<>) :: (Eq a, Num a, DIM n) => SparseVector n a -> SparseVector n a -> a
x <> y = dot x y
