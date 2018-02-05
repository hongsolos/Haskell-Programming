----------------------------------------------------  Sources: http://lpaste.net/56912
--FILE NAME: A2.hs           |* * * * * |##########|		   http://stackoverflow.com
--NAME: Han Hong             | * * * * *|##########|    	   https://mukeshiiitm.wordpress.com
--                           |* * * * * |##########|           https://mail.haskell.org/pipermail/beginners/2011-April/006832.html
--CSCE 314 SECTION 500       |#####################|  		   https://piazza.com/class/ijljhr4dc0w12y?cid=207
--UIN: 824000237             |#####################|		   https://piazza.com/class/ijljhr4dc0w12y?cid=199
----------------------------------------------------

import Data.List

--Chinese Remainder Theorem --------------------------------------------------------------------------
flattenB :: [(Integer, Integer)] -> [Integer]
flattenB [] = []
flattenB ((a,b):xs) = b : flattenB xs

flattenA :: [(Integer, Integer)] -> [Integer]
flattenA [] = []
flattenA ((a,b):xs) = a : flattenA xs

eucd_algo:: Integer -> Integer -> [Integer]
eucd_algo a b | mod a b == 0 = [0,1,b]
        | otherwise = [y,x - y * (div a b), z]
        where
                [x,y,z] = eucd_algo b (mod a b)
 
crt :: [(Integer, Integer)]->(Integer,Integer)
crt ab = let 
                prod = product (flattenB(ab))
                ls = [eucd_algo x (div prod x) !! 1 |x <- flattenB(ab)]
             in (sum[div (x * y * prod) z | (x, y, z) <- zip3 (flattenA(ab)) ls (flattenB(ab))], prod)

--k-composite numbers and angram codes ---------------------------------------------------------------
genFactor :: Integer -> [Integer]
genFactor n = [i | i <-[1..n], (mod n i) == 0]

checkComp :: [Integer] -> Integer -> Bool
checkComp bs a | toInteger(length(bs)) == (a+2) = True
			   | otherwise = False
			   
help :: [Integer] -> Integer -> [Integer]
help [] b = []
help [1] b = []
help (xs) b  | checkComp (genFactor(head xs)) b == True = head xs: help (tail xs) b
			 | otherwise = help (tail xs) b

convert :: Integral t => Integer -> t
convert a = fromIntegral (a)
			 
kcomposite :: Integral t => Integer -> [t]
kcomposite a = map (convert) (help [1..] a)

--Anagram ---------------------------------------------------------------------------------------------
--Encoding
addX :: [Char] -> [Char]
addX a = a ++ "X"

modifyComp :: [Char] -> [Char]
modifyComp a | checkComp (genFactor(toInteger(length(a)))) 2 == True = a
			 | otherwise = modifyComp(addX a)

subEq :: Int -> [a] -> [[a]]
subEq n ls
    | n <= 0 || null ls = []
    | otherwise = take n ls:subEq n (drop n ls)

divideIt :: [Char]->[[Char]]
divideIt a = subEq 7 (modifyComp(a))

encoding :: [[Char]] -> [[Char]]
encoding a = transpose a

makesense :: [[Char]] -> [Char]
makesense [] = []
makesense (x:xs) = x++ makesense xs

anagramEncode :: [Char] -> [Char]
anagramEncode a = makesense(encoding (divideIt a))
--Decoding
divideC :: [Char] -> [[Char]]
divideC a = subEq 3 a

conquerC :: [[Char]] -> [[Char]]
conquerC a = transpose a

removeX :: [Char] -> [Char]
removeX a | last(a) == 'X' = removeX (init a)
		  | otherwise = a

anagramDecode :: [Char] -> [Char]
anagramDecode a = removeX(makesense(conquerC(divideC a)))

--Set -------------------------------------------------------------------------------------------------
isElement :: Eq a => a-> [a]->Bool
isElement q [] = False
isElement q(x:xs) = if q == x then True else isElement q xs

type Set a = [a]

mkSet:: Eq a => [a] -> Set a
mkSet [] = []
mkSet(y:ys)=
			if isElement y ys == True 
			then mkSet ys 
			else y:(mkSet ys)

subset:: Eq a => Set a -> Set a -> Bool
subset [] bs = True
subset (a:as) bs = if isElement a bs then subset as bs else False

setEqual:: Eq a => Set a-> Set a -> Bool
setEqual as bs = (subset as bs)&&(subset bs as)

setProd :: (Eq t, Eq t1) => [t] -> [t1] -> Set (t, t1)
setProd xs ys = [(x,y) | x <- xs, y <- ys]

-- Partition Set (Incomplete) --------------------------------------------------------------------------
partitionSet :: Eq t => Set t -> Set(Set(Set t))
partitionSet [] = [[]]
partitionSet (x:xs) = [[x]:part | part <- partitionSet xs] ++ [(x:ys):yss | (ys:yss) <- partitionSet xs] -- ++ init(partitionSet xs)

bellNum :: Int -> Int
bellNum n = length(partitionSet [1..n])