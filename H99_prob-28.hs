-- 28 a.
lsort :: [String] -> [String]
lsort [] = []
lsort [x] = [x]
lsort xs = merge (lsort ls) (lsort rs)
  where (ls, rs) = halve xs

merge :: [String] -> [String] -> [String]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
  | length x < length y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- 28 b.
lfreq :: String -> [String] -> Int
lfreq x as = length ( filter (\y -> length y == length x) as)

lfsort' :: [String] -> [String] -> [String]
lfsort' _ [] = []
lfsort' _ [x] = [x]
lfsort' as xs = mergef as (lfsort' as ls) (lfsort' as rs)
  where (ls, rs) = halve xs

mergef ::[String] -> [String] -> [String] -> [String]
mergef as [] ys = ys
mergef as xs [] = xs
mergef as (x:xs) (y:ys) 
  | lfreq x as < lfreq y as = x : mergef as xs (y:ys)
  | otherwise = y : mergef as (x:xs) ys

lfsort :: [String] -> [String]
lfsort xs = lfsort' xs xs
