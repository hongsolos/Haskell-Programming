----------------------------------------------------
--FILE NAME: A1.hs           |* * * * * |##########|
--NAME: Han Hong             | * * * * *|##########|
--                           |* * * * * |##########|
--CSCE 314 SECTION 500       |#####################|
--UIN: 824000237             |#####################|
----------------------------------------------------

limerick = do
	let dozen = 12
	let gross = 144
	let score = 20
	let not_a_bit_more = 0
	
	let left = ((dozen+gross+score+6)/7)+55
	let right = 9*9 + not_a_bit_more
	
	if left == right
		then putStrLn "True"
		else putStrLn "False"