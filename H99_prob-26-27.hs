-- 26.
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations 1 xs = [[x] | x <- xs]
combinations n xs = [y:ys | (y,i) <- zip xs [1..], ys <- combinations (n-1) xs, and [not (elem n ys) | n <- take i xs]]

-- alternative solution
combinations' :: Int -> [a] -> [[a]]
combinations' 0 _ = [[]]
combinations' n xs = [xs !! i :x | i <- [0..(length xs)-1], x <- combinations' (n-1) (drop (i+1) xs)]

-- 27.
group :: Eq a => [Int] -> [a] -> [[[a]]]
group [x,y,z] as = [[h,g,f] | h <- combinations' x as, 
			      g <- combinations' y [a | a <- as, not (elem a h)], 
			      f <- combinations' z [a | a <- as, not (elem a h || elem a g)]]


