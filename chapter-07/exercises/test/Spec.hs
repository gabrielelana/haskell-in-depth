import Test.Hspec

import Lib
import Text.Parsec

parseTo :: (Show a, Eq a) => String -> Parsec String () a -> a -> Expectation
parseTo s p a = parse (p <* spaces <* eof) "" s `shouldBe` Right a

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses aritmetic expressions" $ do
      parseTo "(1 + 2)" axpW (OpW ADDW (NumW 1) (NumW 2))
      parseTo "(1+2)" axpW (OpW ADDW (NumW 1) (NumW 2))
      parseTo " ( 1 + 2 ) " axpW (OpW ADDW (NumW 1) (NumW 2))
      parseTo "1" axpW (NumW 1)
      parseTo "foo" axpW (IdW "foo")
      parseTo "f42" axpW (IdW "f42")
    it "parses expressions" $ do
      parseTo "if 1 then 2 else 3 fi" expW (IfW (AxpW (NumW 1)) (AxpW (NumW 2)) (AxpW (NumW 3)))
      parseTo "1 <= 2 " expW (CmpW LTEW (NumW 1) (NumW 2))
      parseTo "not 1 <= 2" expW (NotW (CmpW LTEW (NumW 1) (NumW 2)))
      parseTo "1" expW (AxpW (NumW 1))
      parseTo "x < 10" expW (CmpW LTW (IdW "x") (NumW 10))
    it "parses statements" $ do
      parseTo "foo := 1" parserW (AssignW "foo" (AxpW (NumW 1)))
      parseTo "while x < 10 do foo := 1 done" parserW (WhileW (CmpW LTW (IdW "x") (NumW 10)) (AssignW "foo" (AxpW (NumW 1))))
      parseTo "foo := 1; bar := 2" parserW (SeqW [AssignW "foo" (AxpW (NumW 1)), AssignW "bar" (AxpW (NumW 2))])


main :: IO ()
main = hspec spec
