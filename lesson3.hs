--Wed Jan 25th

--using if

lessSyllyAbs :: Integer -> Integer
lessSyllyAbs n = if n >= 0 then n else -(n)

sillySgn :: Integer -> Integer
sillySgn n = if n == 0 then 0 else 
						if n == 1 then 1 else
						if n == (-1) then (-1) else
									if n < 0 then sillySgn (n+1) else sillySgn (n-1)

ext1 = (1, "hello") -- 2-tuple, pair
ext2 = ("hello again", 1, 5.60, True) -- 4 tuple
ext3 = (5, "hello again",5.60, True) 

f t = (fst t) + 2
g n = (n, n+1, n+2)

keep_going x = keep_going(x-1)
keep_going 0 = "Done"

a = map reverse ["abc", "def", "hi"]
b = map length ["abc", "def", "hij"]

mappair f p = (f (fst p), f (snd p))