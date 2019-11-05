-- 32.
gcd' :: Int -> Int -> Int 
gcd' x y 
  | x < y = gcd y x
  | x `mod` y == 0 = y
  | otherwise = gcd (x-y) y

-- 33.
coprime :: Int -> Int -> Bool
coprime x y = (gcd' x y) == 1

-- 34.
totient :: Int -> Int 
totient 1 = 1
totient m = length (filter (coprime m) [1..m-1])


