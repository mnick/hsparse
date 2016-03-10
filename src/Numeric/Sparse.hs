module Numeric.Sparse
( DIM
, Index
, SparseVector
, SparseMatrix
) where

import Numeric.Sparse.Types
-- import instances from SparseVector
import Numeric.Sparse.Vector ()
-- import instances for SparseMatrix
import Numeric.Sparse.Matrix ()
import Numeric.Sparse.Internal
