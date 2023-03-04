import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "Parse Expression" $ do
    it "will parse a number" $ do
      parseExp "1" `shouldBe` (Right $ Lit 1)
      parseExp "42" `shouldBe` (Right $ Lit 42)
    it "will parse an addition" $ do
      parseExp "1 + 1" `shouldBe` (Right $ (Add (Lit 1) (Lit 1)))
    it "will parse a multiplication" $ do
      parseExp "1 * 1" `shouldBe` (Right $ (Mul (Lit 1) (Lit 1)))
    it "will parse a parenthesized expression" $ do
      parseExp "(1)" `shouldBe` (Right $ Lit 1)
      parseExp "(1 + 1)" `shouldBe` (Right $ (Add (Lit 1) (Lit 1)))
      parseExp "(1 * 1)" `shouldBe` (Right $ (Mul (Lit 1) (Lit 1)))
    it "will associate operations to the left" $ do
      parseExp "1 + 1 + 1" `shouldBe` (Right $ (Add (Add (Lit 1) (Lit 1)) (Lit 1)))
      parseExp "1 * 1 * 1" `shouldBe` (Right $ (Mul (Mul (Lit 1) (Lit 1)) (Lit 1)))
    it "gives precedence to multiplication over addition" $ do
      parseExp "1 + 2 * 3" `shouldBe` (Right $ (Add (Lit 1) (Mul (Lit 2) (Lit 3))))
    it "gives precedence to parenthesized expression over operations" $ do
      parseExp "(1 + 2) * 3" `shouldBe` (Right $ (Mul (Add (Lit 1) (Lit 2)) (Lit 3)))
