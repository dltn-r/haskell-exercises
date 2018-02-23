-- lab 4
-- a bunch of list functions


myAppend :: [a] -> [a] -> [a]
myAppend [] xs = xs
myAppend (y:ys) xs = y:(myAppend ys xs)

myHead :: [a] -> a
myHead [] = error "nope.."
myHead (x:xs) = x

myLast :: [a] -> a
myLast [] = error "nope.."
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "nope.."
myTail (x:xs)  = xs

myInit :: [a] - [a]
myInit [] = error "nope"
myInit [a] = []
myInit (x:xs) = x:(myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ (myConcat xs)

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myProd :: (Num a) => [a] -> a
myProd [] = 0
myProd (x:xs) = x * (myProd xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "nope.."
myMaximum [x] = x
myMaximum (x:xs) if x > myMaximum xs
		 then x
		 else myMaximum xs
		 
myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "nope.."
myMinimum [x] = x
myMinium (x:xs) if x < myMinimum xs
		then x
		else myMinimum xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = if a == x 
		  then True
		  else myElem a xs

myDelete :: (Eq a) => a -> [a] -> [a]
myDelete _ [] = []
myDelete a (x:xs) if a == x
		  then xs
		  else x:(myDelete a xs) 

-- Part 2 --

myUnion :: (Eq a) => [a] -> [a] -> [a]
myUnion (x:xs) (y:ys) if x == y
		      then x:(myUnion xs ys)
		      else myUnion xs ys 










