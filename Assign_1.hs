{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2022
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 - McMaster CS 1JC3 2025.

  Modified by I. Dema 19-JAN-2025.
-}
module Assign_1 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS AND DO NOT ADD ANY IMPORTS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Faris Abuain
-- Date: Friday, January 31st, 2025
macid :: String
macid = "abuainf"

(***) :: Double -> Double -> Double
x *** y = if x >= 0 then x ** y else -( (-x) ** y)

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: Computes the component 'Q' of the function
 - used to determine the roots of a cubic function. 
 - Takes in the values a, b, c from the equation 
 - ax^3 + bx^2 + cx + d = 0
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = (3*a*c - b^2) / (9*(a^2))

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: Computes the component 'R' of the function
 - used to determine the roots of a cubic function. 
 - Takes in the values a, b, c from the equation 
 - ax^3 + bx^2 + cx + d = 0
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9*a*b*c) - (27*(a^2)*d) - 2*(b^3))/(54*(a^3))

{- -----------------------------------------------------------------
 - cubicDiscSign
 - -----------------------------------------------------------------
 - Description: Uses cubicQ and cubicR to compute the sign of the
 - discriminant Q^3 + R^2. If the discriminant is negative, all three 
 - roots are real and distinct. If discriminant is 0, then there are 
 - three real solutions with x2 == x3. x1=x2=x3 is possible. If the 
 - discriminant is positive, x1 is real but x2 and x3 are imaginary. 
 -}
cubicDiscSign :: Double -> Double -> Int
cubicDiscSign q r 
    | q^3 + r^2 < 0 = -1
    | q^3 + r^2 == 0 = 0
    | q^3 + r^2 > 0 = 1

{- -----------------------------------------------------------------
 - cubeRoot
 - -----------------------------------------------------------------
 - Description: Function which computes the cube root of both positive
 - and negative (real) numbers. Uses the fact that the cube root
 - is equivalent to x^1/3. We use the (***) function to correctly
 - handle the cube roots of negative numbers, which are negative. 
 - [** operator would not work!!]
 -}

cubeRoot :: Double -> Double
cubeRoot x = x***(1/3)

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: Calculates the function 'S' which is used to compute
 - the roots of a cubic function, along with 'T'. Calculated by
 - finding the cube root of the sum of R and the square root of
 - the determinant. 
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubeRoot (r + (q^3 + r^2)**(1/2))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: Calculates the function 'T' which is used to compute
 - the roots of a cubic function, along with 'S'. Calculated by
 - finding the cube root of the difference between R and the square 
 - root of the determinant.
 -  
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubeRoot (r - (q^3 + r^2)**(1/2))

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: Function returns an empty list if the equation is 
 - not cubic or if the discriminant is negative, as a negative 
 - discriminant would require complex calculations. 
 - Function returns x1, x2, x3 for discriminant of 0. 
 - This is because when discriminat is 0, S = T. Therefore
 - the third term in both x2 and x3 formulas dissappears, and
 - we have x2 = x3. 
 - In the case of a positive discriminant, x2 =/= x3 and we are
 - only able to compute x1 without using complex computation. 
 -
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
  | a == 0      = []
  | sign == -1  = []
  | sign ==  0  = [s+t-(b/(3*a)), -((s+t)/2) - b/(3*a), -((s+t)/2) - b/(3*a)]
  | sign ==  1  = [s+t-(b/(3*a))]
  | otherwise   = []
  where
    sign = cubicDiscSign q r
    s    = cubicS q r
    t    = cubicT q r
    q    = cubicQ a b c
    r    = cubicR a b c d


(===) :: Double -> Double -> Bool
x === y = let
  tol = 1e-3
  in abs (x-y) <= tol

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

-- Test Case #1: for (***) function.

test_1 = 2***4 === 16

-- Test Cases #2 & 3: for cubicR and cubicQ functions.

test_2 = cubicQ 1 1 1 === (2/9)

test_3 = cubicR 1 1 1 1 === ((-20)/54)

-- Test Cases #s 4 through 6: for cubciDiscSign.

test_4 = cubicDiscSign (-3.0) 1.0 == -1
test_5 = cubicDiscSign 0 0 == 0
test_6 = cubicDiscSign 1 1 == 1

-- Test Case #7: for cubeRoot. 

test_7 = cubeRoot (-27) === (-3)

-- Test Case #8 & 9: for cubicS and cubicT functions

test_8 = cubicS 1 1 === cubeRoot (1 + (1^3 + 1^2)**(1/2))

test_9 = cubicT 1 1 === cubeRoot (1 - (1^3 + 1^2)**(1/2))

-- Test Case #10-14: for cubicRealSolutions.

test_10 = cubicRealSolutions 1 0 0 0 == [0,0,0]

test_11 = cubicRealSolutions 1 0 0 (-8) == [2]

test_12 = cubicRealSolutions 0 1 1 1 == []

test_13 = cubicRealSolutions 1 0 0 1 == [(-1)]

test_14 = cubicRealSolutions 1 0 (-3) 0 == []




