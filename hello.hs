doubleUs x = x + x
divide x y = x / y
multiply x y = x * y
add x y = doubleUs x + doubleUs y
doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
conanO'Brien = "It's a-me, Conan O'Brien!"
circumference :: Float -> Float
circumference r = 2* pi * r
circumference' :: Double -> Double
circumference' r = 2 * pi * r
lucky :: Int -> String
lucky 7 = "LUCKEY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck,pal!"
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
addVectors :: (Double,Double) -> (Double,Double) -> (Double , Double)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
first :: (a,b,c) -> a
first (x,_,_)=x
second :: (a,b,c) -> b
second (_,y,_) = y
third :: (a,b,c) -> c
third(_,_,z)=z
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
tell :: (Show a) => [a] -> String
tell [] = "The list is empry"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++  show x ++ " and " ++ show y
badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

{-
bmiTell :: Double -> String
bmiTell bmi  
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal.Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight,fatty"
  | otherwise   = "You're a whale,congratulations!"
-}

bmiTell :: Double -> Double -> String 
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight , you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal.Pffft, I bet you're ugly!"
  | weight / height - 2 <= 30.0 = "You're fat! Lose some weight,fatty"
  | otherwise 			= "You're a whale, congratulations!"
max' :: (Ord a) => a -> a -> a
max' a b 
 | a <= b = b
 | otherwise = a

myCompare :: (Ord a) => a -> a-> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

bimTell :: Double -> Double -> String
bimTell weight height
 | bmi <= 18.5 = "You're underweight , you emo , you!"
 | bmi <= 25.0 = "You're supposendly normal.Pffft, I bet you're ugly!"
 | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
 | otherwise   = "You're a whale, congratulations!"
 where bmi = weight / height ^ 2

badGreeting :: String
badGreeting = "Oh!Pfft. It's you!"

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you!,"

greet :: String -> String
greet "Juan"     = niceGreeting ++ "Juan!"
greet "Fernando" = niceGreeting ++ "Fernando!"
greet name       = badGreeting ++ " " ++ name
       where niceGreeting = "Hello! So very nice to see you,"
	     badGreeting  = "Oh! Pfft. It's you."
initials :: String -> String -> String 
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
 where (f:_) = firstname
       (l:_) = lastname

calcBmis :: [(Double,Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
 where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h = 
 let sideArea = 2 * pi * r * h
     topArea = pi * r ^ 2
     in sideArea + 2 * topArea
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)

fibNumbers 0 = [1]
fibNumbers 1 = [1,1]
fibNumbers n = fibNumbers (n-1) ++ [fibo n]

fibonacci = 1:1:zipWith (+) fibonacci (tail fibonacci)

replicate' :: Int -> a -> [a]
replicate' n x
 | n <= 0 = []
 | otherwise = x:replicate'(n-1) x

take' :: Int -> [a] -> [a]
take' n _
 | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a(x:xs)
 | a==x = True
 | otherwise = a `elem` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	let smallerOrEqual = [a | a <- xs, a <= x]
	    larger = [a | a <- xs, a > x]
	in  quicksort smallerOrEqual ++ [x] ++ quicksort larger
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z
{-
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x 
-}

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = ( /10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
 where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
 | p x       = x : filter' p xs
 | otherwise = filter' p xs

largestDivisible :: Integer
largestDivisible = head (filter p [10000,9999..])
 where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
 | even n = n : chain (n `div` 2)
 | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
 where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15)
		       (map chain [1..100]))
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

{-
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-}

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

reverse''' :: [a] -> [a]
reverse''' = foldl (flip(:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

{-
(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False
-}
{-
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl (+) (map sqrt [1..])))+1
-}

sum'' :: (Num a) => [a] -> a
sum'' [] = 0
sum'' (x:xs) = x + sum'' xs


