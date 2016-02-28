data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show 
 
makeTree:: Int -> Tree Int
makeTree n = Node n Empty Empty


addNode:: Tree Int -> Int -> Tree Int
addNode  Empty x = makeTree x
addNode (Node item l r ) x = if x == item then (Node item l r) 
	                         else if x > item then  (Node item l (addNode r x)) 
                             else (Node item (addNode l x) r)

findNode :: Int -> Tree Int -> Bool
findNode x Empty  = False
findNode x (Node item l r) = if x == item then True 
	                         else if x > item then findNode x r
	                         else findNode x l


addMultipleNodes:: Tree Int -> [Int] -> Tree Int
addMultipleNodes tree [] = tree
addMultipleNodes Empty xs = Empty
addMultipleNodes tree (x:xs) = addMultipleNodes (addNode tree x) xs

findMin :: Tree Int -> Int
findMin (Node item Empty Empty) = item
findMin (Node item l r) = findMin l


deleteNode:: Tree Int -> Int -> Tree Int
deleteNode Empty _ = Empty
deleteNode (Node item Empty Empty ) x | x == item = Empty
                                      | otherwise = Node item Empty Empty
deleteNode (Node item (Node it1 Empty Empty) Empty) x |  x == item = Node it1 Empty Empty
                                                      | otherwise = Node item (deleteNode (Node it1 Empty Empty) x) Empty

deleteNode (Node item Empty (Node it1 Empty Empty)) x | x == item = Node it1 Empty Empty
                                                      | otherwise = Node item Empty ( deleteNode (Node it1 Empty Empty) x)

deleteNode (Node item l r) x | x == item = Node (findMin r) l (deleteNode r (findMin r))
	                         | x > item  =  Node item l (deleteNode r x)
	                         | otherwise =  Node item (deleteNode l x) r


