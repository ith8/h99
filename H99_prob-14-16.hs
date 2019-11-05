-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- 15
repli :: [a] -> Int -> [a]
repli xs n = concat [replicate n x | x <- xs]

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (x,n') <- zip xs [1..], (n' `mod` n) /= 0]
