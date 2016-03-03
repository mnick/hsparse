{-# LANGUAGE DataKinds            #-}

import Test.Hspec
import Test.QuickCheck
import Numeric.Sparse.Vector (SparseVector, (<>))
import Numeric.Sparse.Matrix (SparseMatrix, (|>), (<|), (***))
import qualified Numeric.Sparse.Vector as V
import qualified Numeric.Sparse.Matrix as M

main :: IO ()
main = hspec $ do

  describe "Sparse.Vector" $ do
    it "dot" $ do
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

    it "scale" $ do
       let x  = V.fromList [(1, 2.0), (4, 3.0)] :: (SparseVector 10 Double)
       let y  = V.fromList [(1, 1.0), (4, 1.5)] :: (SparseVector 10 Double)

       V.scale 0.5 x `shouldBe` y
       V.scale 1 x   `shouldBe` x
       V.scale 0 x   `shouldBe` V.empty
 
    it "norm" $ do
       let x  = V.fromList [(1,2.0), (2,2.0), (4,1.0)] :: (SparseVector 10 Double)

       V.norm x                                 `shouldBe` 3
       V.norm (V.empty :: SparseVector 3 Double) `shouldBe` 0

  describe "Sparse.Matrix" $ do
    it "equality" $ do
      let m  = M.fromList [((2,4),0.5), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)
      let m' = M.fromList [((2,4),0.5), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)
      let x  = M.fromList [((1,4),0.5), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)
      let y  = M.fromList [((2,1),0.5), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)
      let z  = M.fromList [((2,4),0.2), ((7,3), 0.1)] :: (SparseMatrix 10 7 Double)

      m == m' `shouldBe` True
      m == x  `shouldBe` False

    it "mult" $ do
      let x = M.fromList [ ((1,1), 1), ((1,2), 2)
                         , ((2,1), 3), ((2,2), 4)
                         , ((3,1), 5), ((3,2), 6)] :: SparseMatrix 3 2 Double
      let y = M.fromList [ ((1,1), 7), ((1,2), 8)
                         , ((2,1), 9), ((2,2), 10)] :: SparseMatrix 2 2 Double

      let z = M.fromList [ ((1,1), 25), ((1,2), 28)
                         , ((2,1), 57), ((2,2), 64)
                         , ((3,1), 89), ((3,2), 100)] :: SparseMatrix 3 2 Double
      (x *** y) `shouldBe` z
