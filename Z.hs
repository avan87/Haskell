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

toInt:: N -> Int
toInt Z = 0 
toInt (Succ x) = 1 + toInt x

toN:: Int -> N
toN x | x == 0 = Z
      |otherwise =  Succ $ toN (x - 1)

