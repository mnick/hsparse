{-# LANGUAGE DataKinds            #-}

import Test.Hspec
import Test.QuickCheck
import Numeric.LinearAlgebra.Sparse.Vector (SparseVector, (<>))
import Numeric.LinearAlgebra.Sparse.Matrix (SparseMatrix, (|>), (<|))
import qualified Numeric.LinearAlgebra.Sparse.Vector as V
import qualified Numeric.LinearAlgebra.Sparse.Matrix as M

main :: IO ()
main = hspec $ do

  describe "Sparse.Vector" $ do
    it "dot product" $ do
      let x = V.fromList [(1, 2.0), (4, 3.0)] :: (SparseVector 10 Double)
      let y = V.fromList [(1, 3.0), (6, 3.0)] :: (SparseVector 10 Double)
      let z = V.fromList [(2, 3.0), (6, 3.0)] :: (SparseVector 10 Double)
      x <> y       `shouldBe` 6.0
      x <> z       `shouldBe` 0
      y <> z       `shouldBe` 9.0
      x <> V.empty `shouldBe` 0

    it "equality" $ do
      let x  = V.fromList [(1, 2.0), (4, 3.0)] :: (SparseVector 10 Double)
      let x' = V.fromList [(1, 2.0), (4, 3.0)] :: (SparseVector 10 Double)
      let y  = V.fromList [(1, 3.0), (6, 3.0)] :: (SparseVector 10 Double)
      let z  = V.fromList [(4, 3.0)] :: (SparseVector 10 Double)

      x == x'      `shouldBe` True
      x == y       `shouldBe` False
      x == z       `shouldBe` False
      x == V.empty `shouldBe` False

  describe "Sparse.Matrix" $ do
    it "equality" $ do
      let m  = M.fromList [((2,4),0.5), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)
      let m' = M.fromList [((2,4),0.5), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)
      let x  = M.fromList [((1,4),0.5), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)
      let y  = M.fromList [((2,1),0.5), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)
      let z  = M.fromList [((2,4),0.2), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)

      m == m' `shouldBe` True
      m == x  `shouldBe` False
