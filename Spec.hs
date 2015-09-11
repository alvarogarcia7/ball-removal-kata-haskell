import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck property_true 
  quickCheck property_getCandidates_does_not_include_the_first_element
  quickCheck property_getCandidates_does_not_include_the_last_element
  hspec $ do
    describe "Canary Test" $ do
        it "should be green" $ do
          True `shouldBe` True

    describe "toDirection" $ do
        it "parse a string" $ do
	  toDirection "123<<>>..3" `shouldBe` [N,N,N,L,L,R,R,N,N,N]

    describe "get candidates" $ do
        it "of a valid array" $ do
	  getCandidates [L,L,L] `shouldBe` [1]

        it "of a partially-consumed array" $ do
	  getCandidates [L,N,L] `shouldBe` []


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
getCandidates xs = [i|i<-[1..(length xs)-2], xs !! i /= N]


-- Test

property_getCandidates_does_not_include_the_first_element :: [Direction] -> Bool
property_getCandidates_does_not_include_the_first_element xs = not $ 0 `elem` (getCandidates xs)

property_getCandidates_does_not_include_the_last_element :: [Direction] -> Bool
property_getCandidates_does_not_include_the_last_element xs = not $ ((length xs)-1) `elem` (getCandidates xs)

instance Arbitrary Direction where
    arbitrary = oneof $ map return [L, N, R]
