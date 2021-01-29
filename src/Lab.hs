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

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem y (x:xs) | x == y = True
              | otherwise = elem y xs

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs) | x > maximum xs = x
               | otherwise = maximum xs

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse a (x:xs) = x : a : intersperse a xs

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ [x:y | y <- subsequences xs]


--------------------------------------------------------------------------------
