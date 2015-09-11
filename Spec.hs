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
	  getCandidates [L,R,R,L] `shouldBe` [1,2]

        it "of a partially-consumed array" $ do
	  getCandidates [L,N,L] `shouldBe` []
	  getCandidates [N,N,L,L] `shouldBe` []
	  getCandidates [N,N,L,R,L] `shouldBe` [3]
	  getCandidates [N,N,L,R,R,L] `shouldBe` [3,4]

    describe "is finished" $ do
        it "should detect a finished array" $ do
	  isFinished [L,N] `shouldBe` True 
	  isFinished [N,L,N] `shouldBe` True 
	  isFinished [N,N,L,N] `shouldBe` True 
	  isFinished [N,N,L] `shouldBe` True 
	  isFinished [L,L,N] `shouldBe` True

        it "should detect an unfinished array - 1" $ do
	  isFinished [L,L,L] `shouldBe` False
        it "should detect an unfinished array - 2" $ do
	  isFinished [L,N,L,L] `shouldBe` False
        it "should detect an unfinished array - 3" $ do
	  isFinished [L,L,N,L] `shouldBe` False

    describe "remove elements" $ do
        it "should remove the last element, L" $ do
	  removeOne 2 [L,R,L] `shouldBe` [L, N, N]

        it "should remove the first element, L" $ do
	  removeOne 0 [R,R,L] `shouldBe` [N, N, L]

        it "should remove the middle element, R" $ do
	  removeOne 1 [L,R,L] `shouldBe` [L, N, N]

        it "should remove the middle element, L" $ do
	  removeOne 1 [L,L,L] `shouldBe` [N, N, L]

        it "should remove the middle element, R" $ do
	  removeOne 1 [L,R,N,L] `shouldBe` [L, N, N, N]

        it "should remove the middle element, L" $ do
	  removeOne 3 [L,R,N,L] `shouldBe` [L, N, N, N]

    describe "remove balls" $ do
        it "case 1" $ do
	  removeBall "<<>" `shouldBe` "..o"

	it "case 2" $ do
	  removeBall "<>>" `shouldBe` "o.."

	it "case 3" $ do
	  removeBall "<><" `shouldBe` "o.."

	it "case 4" $ do
	  removeBall "<<<" `shouldBe` "..o"

	it "case 5" $ do
	  removeBall ">>><<" `shouldBe` "o...o"

	it "case 6" $ do
	  removeBall "<<><<" `shouldBe` "....o"

	it "case 7" $ do
	  removeBall "<><<><>" `shouldBe` "o.....o"
	 
	--it "case 8" $ do
	  --removeBall ">>><<<>>>>><<<>" `shouldBe` "o.....o.o.....o"
	 
property_true :: Bool -> Bool
property_true _ = True

data Direction = L | R | N deriving (Show, Eq)
type Directions = [Direction]

isFinished :: Directions -> Bool
isFinished xs = (length $ getCandidates xs) `elem` [0]

fromDirection :: Directions -> String
fromDirection xs = map fromDirection' xs
fromDirection' N = '.'
fromDirection' _ = 'o'

toDirection :: String -> [Direction]
toDirection = map parseDirection

parseDirection :: Char -> Direction
parseDirection '<' = L
parseDirection '>' = R
parseDirection x = N

getCandidates :: [Direction] -> [Int]
getCandidates xs = [(getAllNonN xs) !! i|i<-[1..(length $ getAllNonN xs)-2]]

getAllNonN xs = [i|i<-[0..(length xs)-1], xs !! i /= N]

removeOne :: Int -> Directions -> Directions
removeOne idx dir = let cur = dir !! idx in
    removeAnother cur idx $ putNone idx dir

putNone idx dir =  replaceAtIndex idx N dir

removeAnother N idx dir = dir
removeAnother L idx dir = replaceAtIndex (last $ filter (idx >) $ getAllNonN dir) N dir
removeAnother R idx dir = replaceAtIndex (head $ filter (idx <) $ getAllNonN dir) N dir


replaceAtIndex idx item xs = a ++ (item:b)
    where (a, (_:b)) = splitAt idx xs

removeBall :: String -> String
removeBall xs = fromDirection $ removeBall' $ toDirection xs

removeBall' :: Directions -> Directions
removeBall' xs = if isFinished xs
                 then xs
		 else foldl1 merg $ removeBall'' xs
		 
removeBall'' xs = map (\x-> removeBall' $ removeOne x xs) $ getCandidates xs 

merg [] _ = []
merg _ [] = []
merg (x:xs) (x':xs') = unio x x' : merg xs xs'

unio L _ = L
unio R _ = R
unio _ L = L
unio _ R = R
unio N N = N

-- Test

property_getCandidates_does_not_include_the_first_element :: [Direction] -> Bool
property_getCandidates_does_not_include_the_first_element xs = not $ 0 `elem` (getCandidates xs)

property_getCandidates_does_not_include_the_last_element :: [Direction] -> Bool
property_getCandidates_does_not_include_the_last_element xs = not $ ((length xs)-1) `elem` (getCandidates xs)

instance Arbitrary Direction where
    arbitrary = oneof $ map return [L, N, R]
