import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

data Car = Car { company :: String
		,model :: String
		,year :: Int
		}deriving(Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector(i*m) (j*m) (k*m)

data Person = Person {
			firstName :: String
			,lastName :: String
			, age :: Int
			}deriving(Eq,Show,Read)
mikeD = Person {firstName = "Michael" , lastName = "Diamond", age=43 }
adRock = Person {firstName = "Adam" , lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName = "Yauch",age = 44}


