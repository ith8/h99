compress :: Eq a => [a] -> [a]
compress [x] = [x]
compress [] = []
compress (x:ys) 
  | x == head ys = compress ys
  | otherwise = x : compress ys
