{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}

module Numeric.LinearAlgebra.Sparse.Matrix where

import           Data.IntMap.Strict                    (IntMap)
import qualified Data.IntMap.Strict                    as M
import qualified Data.List                             as L
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Sparse.Internal
import           Numeric.LinearAlgebra.Sparse.Vector   (SparseVector (SV))
import qualified Numeric.LinearAlgebra.Sparse.Vector   as V


newtype SparseMatrix (n :: Nat) (m :: Nat) a = SM {mat :: IntMap (SparseVector m a)}
                                             deriving (Eq)

instance (DIM n, DIM m) => Functor (SparseMatrix m n) where
  fmap f mx = mx {mat = fmap (fmap f) (mat mx)}

-- Construction ---------------------------------------------------------------

empty :: (DIM r, DIM c) => SparseMatrix r c a
empty = SM M.empty

-- diag :: (Num a, Eq a, DIM m, DIM n) -> [a] -> Maybe (SparseMatrix m n)
-- diag ds = case r == c && length a == r of

unsafeDiag :: (Num a, Eq a, DIM r, DIM c) => [a] -> SparseMatrix r c a
unsafeDiag ds = L.foldl build empty ids
  where
    ids = L.zip ([1..length ds] :: [Int]) ds
    build m (i, v) = ins m ((i,i), v)

fromList :: (Num a, Eq a, DIM r, DIM c) => [((Int, Int), a)] -> SparseMatrix r c a
fromList = L.foldl' ins empty

add :: (Eq a, Num a, DIM n, DIM m) => SparseMatrix m n a -> SparseMatrix m n a -> SparseMatrix m n a
add (SM x) (SM y) = SM $ M.unionWith (+) x y

row :: (Num a, DIM r, DIM c) => SparseMatrix r c a -> Int -> SparseVector c a
row (SM m) i = M.findWithDefault V.empty i m

-- Map ------------------------------------------------------------------------

mapRows :: (DIM r, DIM c) => (SparseVector c a -> SparseVector c a) -> SparseMatrix r c a -> SparseMatrix r c a
mapRows f (SM m) = SM $ M.filter (\r -> V.nnz r /= 0) $ M.map f m

mapRowsMaybe :: (DIM r, DIM c) => (SparseVector c a -> Maybe (SparseVector c a)) -> SparseMatrix r c a -> SparseMatrix r c a
mapRowsMaybe f (SM m) = SM $ M.mapMaybe f m

-- Modification ---------------------------------------------------------------

modifyRow :: (Num a, DIM r, DIM c) => SparseMatrix r c a -> (SparseVector c a -> SparseVector c a) -> Int -> SparseMatrix r c a
modifyRow mx f i = SM $ M.insert i r' (mat mx)
  where
    r' = f (mx `row` i)

-- | Delete a single element from the matrix
del :: (Num a, DIM r, DIM c) => SparseMatrix r c a -> (Int, Int) -> SparseMatrix r c a
del mx (i,j) = modifyRow mx (`V.del` j) i

-- | Delete a row from the matrix
delRow :: (Num a, DIM r, DIM c) => SparseMatrix r c a -> Int -> SparseMatrix r c a
delRow m i = m {mat = M.delete i (mat m)}

-- | Delete a column from the matrix
delCol :: (Num a, DIM r, DIM c) => SparseMatrix r c a -> Int -> SparseMatrix r c a
delCol m j = mapRowsMaybe (`V.del'` j) m

-- | Insert new element into the matrix. If present, old elements are overwritten.
--   If a zero value is inserted, this element is deleted.
ins :: (Eq a, Num a, DIM r, DIM c) => SparseMatrix r c a -> ((Int, Int), a) -> SparseMatrix r c a
ins m (idx, 0) = del m idx
ins m ((i,j), x) = modifyRow m (\r -> V.ins r (j, x)) i

-- | Matrix transposition (rows become columns)
trans :: (Num a, Eq a, DIM r, DIM c) => SparseMatrix r c a -> SparseMatrix c r a
trans m = M.foldlWithKey' transRow empty (mat m)
  where
    transRow m' i (SV v) = M.foldlWithKey' (transElem i) m' v
    transElem i m' j v = ins m' ((j,i),v)

-- | Sparse matrix vector product.
spMv :: (Eq a, Num a, DIM r, DIM c) => SparseMatrix r c a -> SparseVector c a -> SparseVector r a
spMv (SM m) v = SV (M.mapMaybe (V.spDot v) m)

-- | Sparse vector matrix product. Equivalent to
-- > spMv (trans m) v
spVm :: (Eq a, Num a, DIM r, DIM c) => SparseVector r a -> SparseMatrix r c a -> SparseVector c a
spVm v m = spMv (trans m) v

(|>) :: (Eq a, Num a, DIM r, DIM c) => SparseMatrix r c a -> SparseVector c a -> SparseVector r a
(|>) = spMv

(<|) :: (Eq a, Num a, DIM r, DIM c) => SparseVector r a -> SparseMatrix r c a -> SparseVector c a
(<|) = spVm
