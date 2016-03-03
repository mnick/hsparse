{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Numeric.Sparse.Internal where

import           Data.IntMap
import           GHC.TypeLits

type Index = Key
type DIM = KnownNat

-- | Shifts (re-enumerates) keys of IntMap by given number
shiftKeys :: Int -> IntMap a -> IntMap a
shiftKeys k m = fromAscList [ (i+k,x) | (i,x) <- toAscList m ]


natInt :: (KnownNat n) => proxy n -> Int
natInt = fromIntegral . natVal
