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
                                             deriving (Eq, Show)

instance (DIM n, DIM m) => Functor (SparseMatrix m n) where
  fmap f mx = mx {mat = fmap (fmap f) (mat mx)}

-- Construction ---------------------------------------------------------------

empty :: (DIM r, DIM c) => SparseMatrix r c a
empty = SM M.empty

null :: (DIM r, DIM c) => SparseMatrix r c a -> Bool
null (SM m) = M.null m

nnz :: (DIM r, DIM c) => SparseMatrix r c a -> Int
nnz (SM m) = sum . M.elems $ M.map V.nnz m

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

insertRow :: (Eq a, Num a, DIM r, DIM c) => SparseVector c a -> Index -> SparseMatrix r c a -> SparseMatrix r c a
insertRow v i m | V.null v  = delRow m i
                | otherwise = m {mat = M.insert i v (mat m)}

-- | Matrix transposition (rows become columns)
trans :: (Num a, Eq a, DIM r, DIM c) => SparseMatrix r c a -> SparseMatrix c r a
trans m = M.foldlWithKey' transRow empty (mat m)
  where
    transRow m' i (SV v) = M.foldlWithKey' (transElem i) m' v
    transElem i m' j v = ins m' ((j,i),v)


spMvMaybe :: (Eq a, Num a, DIM r, DIM c) => SparseMatrix r c a -> SparseVector c a -> Maybe (SparseVector r a)
spMvMaybe (SM m) v | M.null r = Nothing
                   | otherwise = Just (SV r)
  where
        r = M.mapMaybe (V.spDot v) m

-- | Sparse matrix-vector product.
multMv :: (Eq a, Num a, DIM r, DIM c) => SparseMatrix r c a -> SparseVector c a -> SparseVector r a
multMv (SM m) v = SV (M.mapMaybe (V.spDot v) m)

-- | Sparse vector-matrix product. Equivalent to
-- > spMv (trans m) v
multVm :: (Eq a, Num a, DIM r, DIM c) => SparseVector r a -> SparseMatrix r c a -> SparseVector c a
multVm v m = multMv (trans m) v

(|>) :: (Eq a, Num a, DIM r, DIM c) => SparseMatrix r c a -> SparseVector c a -> SparseVector r a
(|>) = multMv

(<|) :: (Eq a, Num a, DIM r, DIM c) => SparseVector r a -> SparseMatrix r c a -> SparseVector c a
(<|) = multVm

-- | Sparse matrix-matrix multiplication
mult :: (Eq a, Num a, DIM r, DIM c1, DIM c2) => SparseMatrix r c1 a -> SparseMatrix c1 c2 a -> SparseMatrix r c2 a
mult x y = SM $ M.mapMaybe (spMvMaybe (trans y)) (mat x)

(***) :: (Eq a, Num a, DIM r, DIM c1, DIM c2) => SparseMatrix r c1 a -> SparseMatrix c1 c2 a -> SparseMatrix r c2 a
(***) = mult

-- | Compute Givens rotation of sparse matrix
givens :: (Eq a, Num a, DIM r, DIM c) => (Index, Index) -> (a, a) -> SparseMatrix r c a -> SparseMatrix r c a
givens (i,j) (c,s) m = insertRow (rotate j i c (-s)) j $ insertRow (rotate i j c s) i m
  where
    rotate i' j' c' s' = V.scale c' (row m i') + V.scale s' (row m j')
