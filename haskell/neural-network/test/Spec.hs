import Neuralnet
import Test.Hspec
main :: IO ()
main = hspec $ do
  describe "orN" $ do
    it "should be equivalent to .|." $ do
      x <- [1, 0]
      y <- [1, 0]
      orN x y `shouldBe` x .|. y

  describe "andN" $ do
    it "should be equivalent to .&." $ do
      x <- [1, 0]
      y <- [1, 0]
      andN x y `shouldBe` x .&. y

  describe "nandN" $ do
    it "should be equivalent to .&." $ do
      x <- [1, 0]
      y <- [1, 0]
      andN x y `shouldBe` x .&. y
