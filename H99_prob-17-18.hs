-- 17
split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs x y = drop (x-1) (take y xs)
