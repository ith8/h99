import System.Random

-- 23. 
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do ns <- sequence [randomRIO (0, (length xs) -1) | _ <- [1..n]]
		     return [xs !! x | x <- ns]
		    
-- 24.
diff_select :: Int -> Int -> IO [Int]
diff_select x y = diff_select' x [1..y]

diff_select' :: Int -> [a] -> IO [a]
diff_select' 0 _ = return []
diff_select' x ys = do n <- randomRIO (0, (length ys) -1)
		       rest <- diff_select' (x-1) (except n ys)
		       return ([ys !! n] ++ rest)

except :: Int -> [a] -> [a]
except n ys = (take n ys) ++ (drop (n+1) ys)

-- 25.
rnd_permu :: [a] -> IO [a]
rnd_permu xs = diff_select' (length xs) xs
