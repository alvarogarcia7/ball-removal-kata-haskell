import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck property_true 
  quickCheck property_getCandidates_does_not_include_the_first_element
  hspec $ do
    describe "Canary Test" $ do
        it "should be green" $ do
          True `shouldBe` True

    describe "toDirection" $ do
        it "parse a string" $ do
	  toDirection "123<<>>..3" `shouldBe` [N,N,N,L,L,R,R,N,N,N]

    describe "get candidates" $ do
        it "of a valid array" $ do
	  getCandidates [L,L,L] `shouldBe` [1,2]


property_true :: Bool -> Bool
property_true _ = True

data Direction = L | R | N deriving (Show, Eq)
type Directions = [Direction]

toDirection :: String -> [Direction]
toDirection = map parseDirection

parseDirection :: Char -> Direction
parseDirection '<' = L
parseDirection '>' = R
parseDirection x = N

getCandidates :: [Direction] -> [Int]
getCandidates xs = [1..(length xs)-1]


-- Test

property_getCandidates_does_not_include_the_first_element :: [Direction] -> Bool
property_getCandidates_does_not_include_the_first_element xs = not $ 0 `elem` (getCandidates xs)

instance Arbitrary Direction where
    arbitrary = return L
