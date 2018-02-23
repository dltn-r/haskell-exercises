-- lab 3
-- palindromes, shortests list, adding polynomials

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = if x == reverse x
		 then True
		 else False

shortest :: [[a]] -> [a]
shortest [x] = x
shortest (x:xs) = if length x < length (shortest xs)
		then x
		else shortest xs

type Poly = [Float]
addPolys :: Poly -> Poly -> Poly
addPolys [] p = p
addPolys p [] = p
addPolys p[] = p
addPolys (p:ps) (q:qs) = (p+q):(addPolys ps qs)


