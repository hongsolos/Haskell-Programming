----------------------------------------------------
--FILE NAME: A1.hs           |* * * * * |##########|
--NAME: Han Hong             | * * * * *|##########|
--                           |* * * * * |##########|
--CSCE 314 SECTION 500       |#####################|
--UIN: 824000237             |#####################|
----------------------------------------------------
import Data.Char

--Basic Drill=======================================
increaseTen :: Int -> Int 
increaseTen x = x + 10

circleArea :: (Fractional a) => a -> a
circleArea x = x * 3.14

midList :: [a]->[a]
midList a = tail (init a)

countdownList :: Int -> Int -> [Int]
countdownList a b = [b,  b - (signum $ b - a)..a]

isRight :: Int -> Int -> Int -> Bool
isRight a b c | ((c*c) == ((a*a) + (b*b))) = True
			  | otherwise = False
			 
multComplex :: (Num a) => (a,a)->(a,a)->(a,a,Char)
multComplex (a,b) (d,e) = (a*d-b*e,a*e+b*d,'i')

countChar :: Char -> String -> Int
countChar c s = length $ filter (==c) s

getFirsts :: [(a,a)] -> [a]
getFirsts [] = []
getFirsts ((a,b):xs) = a : getFirsts xs


halfList :: [a] -> [a]
halfList [] = []
halfList xs = head xs : halfList (drop 2 xs)

helper :: Char -> (Bool, Bool, Bool)
helper a | isUpper a == True = (True, False, False)
		 | isLower a == True = (False, True, False)
		 | isDigit a == True = (False, False, True)

uppercaseList :: [Char] -> [(Bool,Bool,Bool)]
uppercaseList xs = map (helper) xs

--Haskell Problems==================================
alterSeries :: (Num a) => [a] -> a
alterSeries[] = 0
alterSeries(x:xs) = x + sum xs - 2*sum(halfList xs)

markDups :: [Char] -> [Char]
markDups [] = []
markDups (x:xs) | elem x xs == True = '_' : markDups xs
			    | otherwise = x : markDups xs
				
helpFold :: [Char] -> String --SWITCH V AND M
helpFold [] = []
helpFold (x:xs) | x == 'v' = 'm' : helpFold xs
				| otherwise = 'v' : helpFold xs

fold :: Int -> String
fold 1 = "v"
fold 2 = "mvv"
fold n = fold(n-1) ++ "v" ++ reverse(helpFold(fold (n-1)))
			 
myMult :: (Integral a) => a -> a -> a
myMult 1 b = b
myMult a b | even a == False = b + myMult (a `div` 2) (b * 2)
		   | otherwise = myMult (a `div` 2) (b*2)