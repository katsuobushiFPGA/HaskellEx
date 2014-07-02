fizzbuzz :: [Int] -> [String]
fizzbuzz [] = []
fizzbuzz (x:xs) 
 | x `mod` 15 == 0 = "FizzBuzz" : fizzbuzz xs
 | x `mod`  3 == 0 = "Fizz" : fizzbuzz xs
 | x `mod`  5 == 0 = "Buzz" : fizzbuzz xs
 | otherwise = show x : fizzbuzz xs

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


