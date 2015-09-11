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

    describe "is finished" $ do
        it "should detect a finished array" $ do
	  isFinished [] `shouldBe` True 
	  isFinished [N] `shouldBe` True 
	  isFinished [L,N] `shouldBe` True 
	  isFinished [N,L,N] `shouldBe` True 
	  isFinished [N,N,L,N] `shouldBe` True 
	  isFinished [N,N,L] `shouldBe` True 

        it "should detect an unfinished array" $ do
	  isFinished [L,L] `shouldBe` False
	  isFinished [N,L,L] `shouldBe` False
	  isFinished [L,N,L] `shouldBe` False
	  isFinished [L,L,N] `shouldBe` False

    describe "remove elements" $ do
        it "should remove the last element, L" $ do
	  removeOne 2 [L,R,L] `shouldBe` [L, N, N]

        it "should remove the first element, L" $ do
	  removeOne 0 [R,R,L] `shouldBe` [N, N, L]

        it "should remove the middle element, R" $ do
	  removeOne 1 [L,R,L] `shouldBe` [L, N, N]

        it "should remove the middle element, L" $ do
	  removeOne 1 [L,L,L] `shouldBe` [N, N, L]

property_true :: Bool -> Bool
property_true _ = True

data Direction = L | R | N deriving (Show, Eq)
type Directions = [Direction]

isFinished :: Directions -> Bool
isFinished xs = (length $ filter (\x->x /= N) xs) `elem` [0, 1]

toDirection :: String -> [Direction]
toDirection = map parseDirection

parseDirection :: Char -> Direction
parseDirection '<' = L
parseDirection '>' = R
parseDirection x = N

getCandidates :: [Direction] -> [Int]
getCandidates xs = [i|i<-[1..(length xs)-2], xs !! i /= N]

removeOne :: Int -> Directions -> Directions
removeOne idx dir = let cur = dir !! idx in
    removeAnother cur idx $ replaceAtIndex idx N dir

removeAnother N idx dir = dir
removeAnother L idx dir = replaceAtIndex (idx - 1) N dir
removeAnother R idx dir = replaceAtIndex (idx + 1) N dir


replaceAtIndex idx item xs = a ++ (item:b)
    where (a, (_:b)) = splitAt idx xs

-- Test

property_getCandidates_does_not_include_the_first_element :: [Direction] -> Bool
property_getCandidates_does_not_include_the_first_element xs = not $ 0 `elem` (getCandidates xs)

property_getCandidates_does_not_include_the_last_element :: [Direction] -> Bool
property_getCandidates_does_not_include_the_last_element xs = not $ ((length xs)-1) `elem` (getCandidates xs)

instance Arbitrary Direction where
    arbitrary = oneof $ map return [L, N, R]
