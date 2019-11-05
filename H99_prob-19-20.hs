-- 19
rotate :: [a] -> Int -> [a]
rotate xs n 
  | n >= 0 = (drop n xs) ++ (take n xs)
  | otherwise = (drop n' xs) ++ (take n' xs)
	where n' = n + length xs

-- 20 
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = ( xs !! (n-1), [x | (x, i) <- zip xs [1..], i /= n])
