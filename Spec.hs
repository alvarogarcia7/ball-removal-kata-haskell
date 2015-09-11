import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck property_true 
  hspec $ do
    describe "Canary Test" $ do
        it "should be green" $ do
          True `shouldBe` True

    describe "toDirection" $ do
        it "parse a string" $ do
	  toDirection "123<<>>..3" `shouldBe` [N,N,N,L,L,R,R,N,N,N]

property_true :: Bool -> Bool
property_true _ = True

data Direction = L | R | N deriving (Show, Eq)

toDirection :: String -> [Direction]
toDirection = map parseDirection

parseDirection :: Char -> Direction
parseDirection '<' = L
parseDirection '>' = R
parseDirection x = N


