data N = Z | Succ N deriving (Eq, Show)


succ' :: N -> N
succ' x = Succ x

prev' :: N -> N
prev' (Succ x) = x

add:: N -> N -> N
add x y | y == Z = x
        | otherwise = add (succ' x) (prev' y)

mult:: N -> N -> N
mult x y | y == Z = Z
         |otherwise = add x (mult x (prev' y) )

minus:: N -> N -> N
minus x y | y == Z = x
          |otherwise = minus (prev' x)(prev' y)

div' :: N -> N -> N -> N
div' x y acc | (lt' x y) == True = x
             | (minus x y) == Z = add acc (Succ Z) 
             | (lt' (minus x y) y) == True = minus x y  
             | otherwise = div' (minus x y) y ( succ' acc)
           
--divWithRem:: N -> N -> N 
--divWithRem
lt':: N -> N -> Bool
lt' Z Z = False
lt' Z _ = True
lt' _ Z = False
lt' x y = lt' (prev' x)(prev' y)


toInt:: N -> Int
toInt Z = 0 
toInt (Succ x) = 1 + toInt x

toN:: Int -> N
toN x | x == 0 = Z
      |otherwise =  Succ $ toN (x - 1)

gcd':: N -> N -> N
gcd' x y | (add Z (mult y (div' x y Z))) == x = y
        | otherwise = gcd' y  (div' x y Z)


