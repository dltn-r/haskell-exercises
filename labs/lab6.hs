-- Lab 6
-- avl trees

data AVLTree t = Empty | Root t (AVLTree t) (AVLTree t) Int
                 deriving (Eq, Ord, Show)


addAVL :: (Ord a) => a -> AVLTree a -> AVLTree a

addAVL x Empty = Root x Empty Empty 0

addAVL x (Root n left right bal_factor)
   | x < n = let newLeft = addAVL x left
             in
                 rebalance (Root n newLeft right ((height newLeft) - (height right)))
   | otherwise = let newRight = addAVL x right
                 in
                     rebalance (Root n left newRight ((height left) - (height newRight)))


getRoot :: AVLTree a -> a

getRoot Empty = error "getRoot from Empty"

getRoot (Root a _ _ _) = a


getLeft :: AVLTree a -> AVLTree a

getLeft Empty = error "getLeft from Empty"

getLeft (Root _ left _ _) = left


getRight :: AVLTree a -> AVLTree a

getRight Empty = error "getRight from Empty"

getRight (Root _ _ right _) = right


getBF :: AVLTree a -> Int

getBF Empty = 0

getBF (Root _ _ _ bf) = bf


height :: AVLTree a -> Int

height Empty = 0

height (Root _ left right _) = 1 + (max (height left) (height right))


rebalance :: AVLTree a -> AVLTree a

rebalance Empty = Empty

rebalance (Root r Empty Empty bf) = Root r Empty Empty bf

rebalance (Root r left right bf)
    | bf == -2 && rbf == -1 = let rl = (getLeft right)
                              in
                                  (Root (getRoot right)                                                               -- right right
                                        (Root r left rl ((height left) - (height rl)))
                                        (getRight right)
                                        ((1 + (max (height left) (height rl))) - (height (getRight right)))
                                  )
    | bf == -2 && rbf == 1 = let rl = getLeft right
                                 rr = getRight right
                             in
                                 (Root (getRoot (rl))                                                                 -- right left
                                       (Root r left (getLeft rl) ((height left) - (height (getLeft rl))))
                                       (Root (getRoot right) (getRight rl) rr ((height (getRight rl)) - (height rr)))
                                       ((max (height left) (height (getLeft rl))) - (max (height (getRight rl)) (height rr)))
                                 )
    | bf == 2 && lbf == 1 = let lr = getRight left
                            in
                                (Root (getRoot left)                                                                  -- left left
                                      (getLeft left)
                                      (Root r lr right ((height lr) - (height right)))
                                      ((height (getLeft left)) - (1 + (max (height lr) (height right))))
                                )
    | bf == 2 && lbf == -1 = let lr = getRight left
                                 ll = getLeft left
                             in
                                 (Root (getRoot lr)                                                                   -- left right
                                       (Root (getRoot left) ll (getLeft lr) ((height ll) - (height (getLeft lr))))
                                       (Root r (getRight lr) right ((height (getRight lr)) - (height right)))
                                       ((max (height ll) (height (getLeft lr))) - (max (height(getRight lr)) (height right)))
                                 )
    | otherwise = (Root r left right bf)
    where rbf = getBF right
          lbf = getBF left
                                   

makeAVL :: (Ord a) => [a] -> AVLTree a

makeAVL [] = Empty

makeAVL (x:xs) = addAVL x (makeAVL xs)


