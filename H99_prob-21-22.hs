-- 21.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)

-- 22.
range :: Int -> Int -> [Int]
range x y = [x..y]
