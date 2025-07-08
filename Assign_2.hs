{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) William M. Farmer 2024
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2025
-}
module Assign_2 where

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

-- Name: Faris Abuain
-- Date: March 2nd, 2025
macid :: String
macid = "abuainf"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description:
 -   Returns the real component of a Gaussian integer.
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (a, _) = a 

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description:
 -   Returns the imaginary component of a Gaussian integer.
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (_, b) = b

{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description:
 -   Returns the conjugate of a Gaussian integer, i.e. returns a
 -   Gaussian integer "a - bi" based on input integer "a + bi".
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj (a, b) = (a, -b)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description:
 -   Returns the sum of two Gaussian integers.
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd (a, b) (c, d) = (a+c, b+d) 


{- -----------------------------------------------------------------
 - gaussMul
 - -----------------------------------------------------------------
 - Description:
 -   Returns the product of two Gaussian integers.
 -}
gaussMul :: GaussianInt -> GaussianInt -> GaussianInt
gaussMul (a, b) (c,d) = (a*c - b*d, a*d + b*c)


{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description:
 -   The norm of a Gaussian integer is defined as the product of the
 -   integer, a + bi, and it's conjugate, a - bi, which returns the 
 -   (real) integer a^2 + b^2.
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm (a,b) = a^2 + b^2

{- -----------------------------------------------------------------
 - gaussAddList
 - -----------------------------------------------------------------
 - Description:
 -   Returns the sum of all Gaussian integers in a list of Gaussian 
 -   integers. Returns 0 {i.e. 0 + 0i} for empty list.
 -}
gaussAddList :: [GaussianInt] -> GaussianInt
gaussAddList [] = (0,0) 
gaussAddList (x:xs) = gaussAdd x (gaussAddList xs)

{- -----------------------------------------------------------------
 - gaussMulList
 - -----------------------------------------------------------------
 - Description:
 -   Returns the product of all Gaussian integers in a list of Gaussian 
 -   integers. Returns 1 {i.e. 1 + 0i} for empty list.
 -}
gaussMulList :: [GaussianInt] -> GaussianInt
gaussMulList [] = (1,0) 
gaussMulList (x:xs) = gaussMul x (gaussMulList xs)

{- ------------------------------------------------------------------------
 - gaussCircle
 - ------------------------------------------------------------------------
 - Description:
 -   Returns a list of Gaussian integers whose norm is stricly less than
 -   a given integer 'n' from an input list of Gaussian integers.
 -}
gaussCircle :: [GaussianInt] -> Integer -> [GaussianInt]
gaussCircle [] _ = []
gaussCircle (x:xs) n 
    | gaussNorm x < n = x : gaussCircle xs n
    | otherwise = gaussCircle xs n

{-
TEST PLAN

Function: gaussConj
Test Case Number: 1
Input: gaussConj (3, 4)
Expected Output: (3, -4)
Actual Output: (3, -4)

Test Case Number: 2
Input: gaussConj (-2, 5)
Expected Output: (-2, -5)
Actual Output: (-2, -5)

Test Case Number: 3
Input: gaussConj (0, -7)
Expected Output: (0, 7)
Actual Output: (0, 7)

Function: gaussAdd
Test Case Number: 1
Input: gaussAdd (3,4) (1,2)
Expected Output: (4,6)
Actual Output: (4,6)

Test Case Number: 2
Input: gaussAdd (-1, 5) (2, -3)
Expected Output: (1,2)
Actual Output: (1,2)

Test Case Number: 3
Input: gaussAdd (0,0) (0,0)
Expected Output: (0,0)
Actual Output: (0,0)

Function: gaussMul
Test Case Number: 1
Input: gaussMul (1,1) (1,1)
Expected Output: (0,2)
Actual Output: (0,2)

Test Case Number: 2
Input: gaussMul (2,3) (4,-1)
Expected Output: (11,10)
Actual Output: (11,10)

Test Case Number: 3
Input: gaussMul (0,5) (0,5)
Expected Output: (-25,0)
Actual Output: (-25,0)

Function: gaussNorm
Test Case Number: 1
Input: gaussNorm (3,4)
Expected Output: 25
Actual Output: 25

Test Case Number: 2
Input: gaussNorm (-2, -2)
Expected Output: 8
Actual Output: 8

Test Case Number: 3
Input: gaussNorm (0,0)
Expected Output: 0
Actual Output: 0

Function: gaussAddList
Test Case Number: 1
Input: gaussAddList [(1,2), (3,4), (5,6)]
Expected Output: (9,12)
Actual Output: (9,12)

Test Case Number: 2
Input: gaussAddList []
Expected Output: (0,0)
Actual Output: (0,0)

Test Case Number: 3
Input: gaussAddList [(2,-3), (-2,3)]
Expected Output: (0,0)
Actual Output: (0,0)

Function: gaussMulList
Test Case Number: 1
Input: gaussMulList [(1,1), (1,1), (1,1)]
Expected Output: (-2,2)
Actual Output: (-2,2)

Test Case Number: 2
Input: gaussMulList []
Expected Output: (1,0)
Actual Output: (1,0)

Test Case Number: 3
Input: gaussMulList [(2,0), (3,0), (4,0)]
Expected Output: (24,0)
Actual Output: (24,0)

Function: gaussCircle
Test Case Number: 1
Input: gaussCircle [(3,4), (1,1), (0,2)] 5
Expected Output: [(1,1), (0,2)]
Actual Output: [(1,1), (0,2)]

Test Case Number: 2
Input: gaussCircle [(0,0), (3,3), (-4,-4)] 20
Expected Output: [(0,0), (3,3)]
Actual Output: [(0,0), (3,3)]]

Test Case Number: 3
Input: gaussCircle [(5,12), (6,8), (1,1)] 50
Expected Output: [(1,1)]
Actual Output: [(1,1)]
-}

