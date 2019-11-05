{- Haskell practice taken from 99-Haksell problems (https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems), which were translated form 99-Lisp and 99-Prolog.
-}
-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast xs = xs !! (length xs -2)

-- 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- 4
myLength :: [a] -> Int 
myLength xs = sum [ 1 | _ <- xs]

-- 5 
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = and [ x == x' | (x, x') <- (zip xs (reverse xs))] 

-- Original problems at https://wiki.haskell.org/99_questions/1_to_10