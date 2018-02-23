-- lab 5
-- binary trees and sorting

data BinTree t = Empty | Root t (BinTree t) (BinTree t)
		 deriving (Eq, Ord, Show)

leaf :: a -> Bintree a
leaf x = Root x Empty Empty

addNode :: (Ord a) => a -> BinTree a -> BinTree a
addNode _ Empty = leaf a
addNode a (Root x l r) = if a < l 
			 then Root x(addNode a l)
			 else addNode a r

makeTree :: (Ord a) => [a] -> BineTree a
makeTree [] = Empty 
makeTree (x:xs) = addNode x (makeTree xs)

inOrder :: BinTree a -> [a]
inOrder Empty = []
inOrder (Root x l r) = inOrder l ++ [x] ++ inOrder r

mpsort :: (Ord a) => [a] -> [a]
mpsort [] = []
mpsort x = inOrder(makeTree x)

hosort :: (a -> a -> bool) -> [a] -> [a]
hosort f [] = []
hosort f (x:xs) = hosort f [a | a <- xs, f a x] ++ [x] ++ hosort f [a | a <- xs, not(f a x)]
