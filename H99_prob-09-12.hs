-- 9
split :: [a] -> [[a]]
split [] = []
split [x] = [[x]]
split (x:xs) = [x]:(split xs)

merge :: Eq a => [[a]] -> [[a]]
merge [] = []
merge [xs] = [xs]
merge (xs:xss)
  | head xs == head (head xss) = merge ((xs ++ (head xss)):(tail xss))
  | otherwise = xs:(merge xss)

pack :: Eq a => [a] -> [[a]]
pack xs = merge (split xs)

-- 10.  Run-length encoding
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [( length x, head x) | x <- pack xs]

-- 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = [ y | x <- pack xs, let y =  if length x > 1 
					     then  Multiple (length x) (head x) 
					     else  Single (head x) ] 
-- 12
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs)  = (x:decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs)

