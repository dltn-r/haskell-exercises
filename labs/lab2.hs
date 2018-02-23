
-- lab 2
-- working with triangle areas

isSum :: Int-> Int-> Int-> Bool
isSum x y z
	| x+y == z = True
     	| x+z == y = True
     	| y+z == x = True
     	| otherwise = False 

isTriangle :: Float -> Float -> Float -> Bool
isTriangle x y z 
        | x+y > z = True
	| x+z > y = True
	| y+z > x = True
	| otherwise False


triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = if (isTriangle a b c)
		     then let s = (a+b+c)/2
			  in sqrt (s * (s-a) * (s-b) * (s-c))
		     else error "Nope!"
