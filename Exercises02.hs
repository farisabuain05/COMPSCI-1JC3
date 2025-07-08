{-|
Module      : HaskellExercises02.Exercises02
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 02 - McMaster CS 1JC3 2025
-}
module Exercises02 where

import Prelude hiding ((||),(&&),abs)

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------
macid = "abuainf"

-- NOTE see the wikipedia page on truth tables https://en.wikipedia.org/wiki/Truth_table
--      (patricularly the sections Logical conjuction, Logical disjunction, etc) for a reference
--      on the different boolean operators

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement Logical nor operation (see slides) using pattern matching
-- NOTE feel free to change the function declaration (pattern match on the arguments), just don't change
--      the type decleration
-----------------------------------------------------------------------------------------------------------
nor :: Bool -> Bool -> Bool
nor x y = 
  case (x, y) of
    (False, False) -> True
    _ -> False

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement nand operation using pattern matching
-----------------------------------------------------------------------------------------------------------
nand :: Bool -> Bool -> Bool
nand x y = 
  case (x, y) of
    (True, True) -> False
    _ -> True

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement Logical equivalence using pattern matching
-----------------------------------------------------------------------------------------------------------
(<==>) :: Bool -> Bool -> Bool
x <==> y | x == y = True
         | otherwise = False

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the function abs that returns the absolute value of a number
-----------------------------------------------------------------------------------------------------------
abs :: (Num a,Ord a) => a -> a
abs x | x < 0 = -x
      | otherwise = x

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement a not equal (=/=) function that compares two floating point numbers, and returns True if their
-- distance is greater than 1e-3
-- NOTE use the abs fine you just defined
-----------------------------------------------------------------------------------------------------------
(=/=) :: (Floating a,Ord a) => a -> a -> Bool
x =/= y = abs(x - y) > 1e-3

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement a function insFront that takes the first element of a list and inserts it
-- to the front of the list, thus duplicating the first element of it
-----------------------------------------------------------------------------------------------------------
insFront :: [a] -> [a]
insFront (x:xs) = x : x : xs

-- Exercise G
-----------------------------------------------------------------------------------------------------------
-- Implement a function mod3 that takes a list of integers and computes the remainder of division of
-- each element of the list by 3 (using the modulo operator `mod`)
-- NOTE use the map function combined with a lambda expression to do the division
-----------------------------------------------------------------------------------------------------------
mod3 :: Integral a => [a] -> [a]
mod3 xs = map (\a -> mod a 3) xs
