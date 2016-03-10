{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Sparse.Types where

import           Data.IntMap             (IntMap)
import           Data.Proxy
import           GHC.TypeLits

import           Numeric.Sparse.Internal

-- Sparse vector --------------------------------------------------------------

data SparseVector (n :: Nat) a = SV {vec :: !(IntMap a)}
                                  deriving (Eq, Read)

instance (DIM n) => Functor (SparseVector n) where
  fmap f v = v {vec = fmap f (vec v)}

instance (DIM n) => Foldable (SparseVector n) where
  foldr f d v = foldr f d (vec v)

instance (Show a, Eq a, Num a, DIM n) => Show (SparseVector n a) where
  show (SV v) = show (natInt (Proxy :: Proxy n)) ++ ": " ++ show v

-- Sparse matrix --------------------------------------------------------------

data SparseMatrix (n :: Nat) (m :: Nat) a = SM {mat :: !(IntMap (SparseVector m a))}
                                          deriving (Eq, Show)

instance (DIM n, DIM m) => Functor (SparseMatrix m n) where
  fmap f mx = mx {mat = fmap (fmap f) (mat mx)}
