-- Friday Jan 20

a = 15 
b = 3
foo x = 50 * x
foo' x y = sqrt (x^2 + y^2) --This raises type mismatch errors

cube :: Integer -> Integer --Map integer with integer
cube x = x^3 --Cube 2 works just fine, (-2)

pow9 x = cube(cube x)