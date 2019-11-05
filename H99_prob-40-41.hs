factors :: Int -> [Int]
factors n = filter (\x -> n `mod` x == 0) [1..n]

isPrime :: Int -> Bool 
isPrime n = factors n == [1,n]

-- 40.
goldbach :: Int -> (Int, Int)
goldbach n = head [(x,n-x) | x <- ps, elem (n-x) ps]
  where ps = filter isPrime [1..n]

-- 41.
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList x y = map goldbach (filter even [x..y])

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' x y lb = [(a,b) | (a,b) <- goldbachList x y, a > lb && b > lb]
