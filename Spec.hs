import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck property_true 
  hspec $ do
    describe "Canary Test" $ do
        it "should be green" $ do
          True `shouldBe` True

property_true :: Bool -> Bool
property_true _ = True

data Direction = L | R | N

parseDirection :: Char -> Direction
parseDirection '<' = L
parseDirection '>' = R
parseDirection x = N
