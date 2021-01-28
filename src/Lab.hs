--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Recursive functions                                                   --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, subsequences )

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

-- elem :: Eq a => a -> [a] -> Bool
-- elem _ [] = False
-- elem y (x:xs)
--     | y==x = True
--     | otherwise = elem y xs 

-- elem :: Eq a => a -> [a] -> Bool
-- elem _ [] = False
-- elem y (x:xs) =
--     if y==x then True else elem y xs

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem y (x:xs) = y==x || elem y xs 

-- maximum :: Ord a => [a] -> a
-- maximum [x] = x
-- maximum (x:xs)
--     | x > y = x
--     | otherwise = y
--     where y = maximum xs

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

-- intersperse '|' "" => ""
-- intersperse '|' "D" => "D"
-- intersperse '|' "DUCK" => "D|U|C|K"
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse s (x:xs) = x : s : intersperse s xs

-- subsequences [] => [[]]
-- subsequences [1] => [[], [1]]
-- subsequences [1,2] => [[], [1], [2], [1,2]]

-- subsequences :: [a] -> [[a]]
-- subsequences [] = [[]]
-- subsequences (x:xs) = ys ++ [x:y | y <- ys]
--     where ys = subsequences xs

-- subsequences :: [a] -> [[a]]
-- subsequences [] = [[]]
-- subsequences (x:xs) = ys ++ map (\y -> x:y) ys
--     where ys = subsequences xs

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = ys ++ map (x:) ys
    where ys = subsequences xs

--------------------------------------------------------------------------------
