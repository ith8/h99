-- 31.
factors :: Int -> [Int]
factors n = filter (\x -> n `mod` x == 0) [1..n]

isPrime :: Int -> Bool 
isPrime n = factors n == [1,n]

-- 35.
primeFactors' :: Int -> [Int]
primeFactors' n = [x | x <- factors n, isPrime x]

primeFactors :: Int -> [Int]
primeFactors n 
  | product ps == n = ps
  | isPrime r = r:ps
  | otherwise = (primeFactors r) ++ ps
  where ps = primeFactors' n
	r = n `div` (product (primeFactors' n))

-- 36.
prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult n = [ (p, length (filter (== p) (primeFactors n))) | p <- primeFactors' n]

-- 37.
totient :: Int -> Int
totient n = product [(p-1) * (p^(m-1)) | (p,m) <- prime_factors_mult n]

-- 39.
primesR :: Int -> Int -> [Int]
primesR x y = filter isPrime [x..y]
