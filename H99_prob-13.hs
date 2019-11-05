-- Implement encode without using results from prob 9 (almost)
split :: [a] -> [(Int, a)]
split [] = []
split [x] = [(1,x)]
split (x:xs) = (1,x):(split xs)
    
merge :: Eq a => [(Int, a)] -> [(Int, a)]
merge [] = []
merge [(n,xs)] = [(n,xs)]
merge (x:xs)
  | snd x == snd (head xs) = merge ((fst x + 1, snd x):(tail xs))
  | otherwise = x:(merge xs)

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect xs = [y | (n,x) <- (merge (split xs)), let y = if n > 1 
							       then Multiple n x
							       else Single x]
-- Original problem at: https://wiki.haskell.org/99_questions/11_to_20 
