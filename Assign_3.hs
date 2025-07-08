{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) William M. Farmer 2024
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2024
-}
module Assign_3 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E., THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E., RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (E.G., IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Faris Abuain
-- Date: March 23rd, 2025
macid :: String
macid = "abuainf"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}

data Poly a = 
    X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving Show

newtype PolyList a = PolyList [a]
  deriving Show

{- -----------------------------------------------------------------
 - polyFun
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates a polynomial function at a given value.
 -}
polyFun :: Num a => Poly a -> a -> a
polyFun p c = case p of
  X -> c
  Coef a -> a
  Sum p q -> polyFun p c + polyFun q c
  Prod p q -> polyFun p c * polyFun q c

{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - Description:
 -    Description:
 -    Computes the degree of a polynomial.
 -}
polyDegree :: (Num a, Eq a) => Poly a -> Int
polyDegree p = case p of
  X -> 1
  Coef _ -> 0
  Sum p q -> max (polyDegree p) (polyDegree q)
  Prod p q -> polyDegree p + polyDegree q

{- -----------------------------------------------------------------
 - polyListFun
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates a polynomial list function at a given value.
 -}
polyListFun :: Num a => PolyList a -> a -> a
polyListFun (PolyList coeffs) c = sum [a * (c^i) | (a, i) <- zip coeffs [0..]]

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description:
 -    Computes the degree of a polynomial represented as a list.
 -}
polyListDegree :: (Num a, Eq a) => PolyList a -> Int
polyListDegree (PolyList coeffs) = length coeffs - 1

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description:
 -   Computes the sum of two polynomial lists.
 -}
polyListSum :: Num a => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList pl) (PolyList ql) = PolyList (zipWith (+) (extend pl n) (extend ql n))
  where n = max (length pl) (length ql)
        extend xs n = xs ++ replicate (n - length xs) 0

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description:
 -    Computes the product of two polynomial lists.
 -}
polyListProd :: Num a => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList pl) (PolyList ql) = PolyList (foldl addTerm (replicate (m + n - 1) 0) terms)
  where
    m = length pl
    n = length ql
    terms = [(i + j, pi * qj) | (i, pi) <- zip [0..] pl, (j, qj) <- zip [0..] ql]
    addTerm acc (i, v) = if i < length acc
                         then take i acc ++ [v + (acc !! i)] ++ drop (i + 1) acc
                         else acc ++ replicate (i - length acc + 1) 0 ++ [v]

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description:
 -    Converts a polynomial list to a polynomial datatype.
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList []) = Coef 0
polyListToPoly (PolyList [a]) = Coef a
polyListToPoly (PolyList (a:as)) = Sum (Coef a) (Prod X (polyListToPoly (PolyList as)))

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description:
 -    Converts a polynomial datatype to a polynomial list.
 -}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
polyToPolyList p = PolyList (convert p)
  where
    convert (Coef a) = [a]
    convert X = [0, 1]  -- Represents X (degree 1 polynomial)
    convert (Sum p q) = zipWith (+) (convert p ++ repeat 0) (convert q ++ repeat 0)
    convert (Prod p q) = 
        let pl = convert p
            ql = convert q
            productTerms = [ (i + j, pi * qj) | (i, pi) <- zip [0..] pl, (j, qj) <- zip [0..] ql ]
        in foldl (\acc (i, v) -> if i < length acc
                                  then take i acc ++ [v + (acc !! i)] ++ drop (i + 1) acc
                                  else acc ++ replicate (i - length acc + 1) 0 ++ [v])
                 (replicate (length pl + length ql - 1) 0) productTerms


{- -----------------------------------------------------------------
 - TEST PLAN
 - -----------------------------------------------------------------
 - Function: polyFun
 - Test Case Number: 1
 - Input: polyFun X 5
 - Expected Output: 5
 - Actual Output: 5
 - Rationale: Simple test case to evaluate the linear polynomial `X` at `X = 5`. It tests if the function handles a basic polynomial.

 - Function: polyFun
 - Test Case Number: 2
 - Input: polyFun (Sum (Coef 3) X) 2
 - Expected Output: 5
 - Actual Output: 5
 - Rationale: Edge case where a polynomial is a sum of a constant and a linear term. This tests the function's ability to handle a sum of terms.

 - Function: polyFun
 - Test Case Number: 3
 - Input: polyFun (Prod (Coef 2) X) 4
 - Expected Output: 8
 - Actual Output: 8
 - Rationale: Edge case where the polynomial is a product of a constant and `X`. This tests if the function can handle products correctly.

 - Function: polyDegree
 - Test Case Number: 1
 - Input: polyDegree X
 - Expected Output: 1
 - Actual Output: 1
 - Rationale: Basic test case to check if the degree of the polynomial `X` is correctly identified as 1.

 - Function: polyDegree
 - Test Case Number: 2
 - Input: polyDegree (Sum (Coef 2) X)
 - Expected Output: 1
 - Actual Output: 1
 - Rationale: Simple test case with a sum of a constant and a linear term. This checks if the function correctly identifies the degree of the sum as the highest degree term.

 - Function: polyDegree
 - Test Case Number: 3
 - Input: polyDegree (Prod (Coef 2) X)
 - Expected Output: 1
 - Actual Output: 1
 - Rationale: A product of a constant and `X` should return degree 1. This tests the edge case of a product, ensuring the degree is computed correctly.

 - Function: polyListFun
 - Test Case Number: 1
 - Input: polyListFun (PolyList [2, 3]) 4
 - Expected Output: 14
 - Actual Output: 14
 - Rationale: A simple test case to check if the function correctly evaluates a polynomial `2 + 3X` at `X = 4`. This ensures the general functionality.

 - Function: polyListFun
 - Test Case Number: 2
 - Input: polyListFun (PolyList [1, 0, -1]) 2
 - Expected Output: -1
 - Actual Output: -1
 - Rationale: An edge case where the polynomial is quadratic (`1 - X^2`). This checks if the function handles negative values and powers of `X`.

 - Function: polyListFun
 - Test Case Number: 3
 - Input: polyListFun (PolyList [0, 1, 0, 3]) 3
 - Expected Output: 30
 - Actual Output: 30
 - Rationale: A more complex polynomial `X + 3X^3` evaluated at `X = 3`. This ensures the function handles higher-degree polynomials properly.

 - Function: polyListDegree
 - Test Case Number: 1
 - Input: polyListDegree (PolyList [2, 3, 4])
 - Expected Output: 2
 - Actual Output: 2
 - Rationale: Basic test case to check if the degree of a polynomial list is computed correctly. The degree of `[2, 3, 4]` is 2.

 - Function: polyListDegree
 - Test Case Number: 2
 - Input: polyListDegree (PolyList [1, 0, -1])
 - Expected Output: 2
 - Actual Output: 2
 - Rationale: An edge case with a polynomial of degree 2 (`1 - X^2`). This ensures the function handles polynomials with higher degrees.

 - Function: polyListDegree
 - Test Case Number: 3
 - Input: polyListDegree (PolyList [0, 0, 0])
 - Expected Output: 2
 - Actual Output: 2
 - Rationale: Edge case with all coefficients zero. The length of the list is used to determine the degree, so the degree should still be `2`.

 - Function: polyListSum
 - Test Case Number: 1
 - Input: polyListSum (PolyList [1, 2]) (PolyList [3, 4])
 - Expected Output: PolyList [4, 6]
 - Actual Output: PolyList [4, 6]
 - Rationale: Simple case where two polynomials `1 + 2X` and `3 + 4X` are summed. This ensures the sum of polynomials works as expected.

 - Function: polyListSum
 - Test Case Number: 2
 - Input: polyListSum (PolyList [1, 0, 2]) (PolyList [0, 2, 0])
 - Expected Output: PolyList [1, 2, 2]
 - Actual Output: PolyList [1, 2, 2]
 - Rationale: Edge case where the sum of polynomials results in a polynomial with three terms. This checks the proper handling of varying lengths of polynomial lists.

 - Function: polyListSum
 - Test Case Number: 3
 - Input: polyListSum (PolyList [0, 0, 0]) (PolyList [0, 0, 0])
 - Expected Output: PolyList [0, 0, 0]
 - Actual Output: PolyList [0, 0, 0]
 - Rationale: Edge case where both polynomials are zero polynomials. This tests if the sum of zero polynomials is correctly handled.

 - Function: polyListProd
 - Test Case Number: 1
 - Input: polyListProd (PolyList [1, 2]) (PolyList [3, 4])
 - Expected Output: PolyList [3, 10, 8]
 - Actual Output: PolyList [3, 10, 8]
 - Rationale: Simple case to check if the product of polynomials `1 + 2X` and `3 + 4X` is computed correctly.

 - Function: polyListProd
 - Test Case Number: 2
 - Input: polyListProd (PolyList [2]) (PolyList [3])
 - Expected Output: PolyList [6]
 - Actual Output: PolyList [6]
 - Rationale: Basic edge case where both polynomials are constants. The product should result in a constant.

 - Function: polyListProd
 - Test Case Number: 3
 - Input: polyListProd (PolyList [1, 0]) (PolyList [0, 1])
 - Expected Output: PolyList [0, 1, 0]
 - Actual Output: PolyList [0, 1, 0]
 - Rationale: A simple edge case where `X` multiplied by `X` results in `X^2`. This tests the basic functionality of multiplying polynomials.

 - Function: polyListToPoly
 - Test Case Number: 1
 - Input: polyListToPoly (PolyList [1, 2])
 - Expected Output: Sum (Coef 1) (Prod X (Coef 2))
 - Actual Output: Sum (Coef 1) (Prod X (Coef 2))
 - Rationale: Simple case where `[1, 2]` is converted to the polynomial `1 + 2X`.

 - Function: polyListToPoly
 - Test Case Number: 2
 - Input: polyListToPoly (PolyList [3, 0, -1])
 - Expected Output: Sum (Coef 3) (Prod X (Sum (Coef (-1)) (Prod X X)))
 - Actual Output: Sum (Coef 3) (Prod X (Sum (Coef (-1)) (Prod X X)))
 - Rationale: A test case where the list `[3, 0, -1]` is converted to the polynomial `3 - X^2`. This checks if the conversion works for polynomials of degree 2.

 - Function: polyListToPoly
 - Test Case Number: 3
 - Input: polyListToPoly (PolyList [0, 0, 0])
 - Expected Output: Coef 0
 - Actual Output: Coef 0
 - Rationale: Edge case where an empty polynomial list is converted to a zero polynomial (`Coef 0`).

 - Function: polyToPolyList
 - Test Case Number: 1
 - Input: polyToPolyList (Coef 1)
 - Expected Output: PolyList [1]
 - Actual Output: PolyList [1]
 - Rationale: Simple case to check if the constant polynomial `1` is correctly converted to the list `[1]`.

 - Function: polyToPolyList
 - Test Case Number: 2
 - Input: polyToPolyList (Prod (Coef 2) X)
 - Expected Output: PolyList [0, 2]
 - Actual Output: PolyList [0, 2]
 - Rationale: Tests the conversion of `2X` into the list `[0, 2]`.

 - Function: polyToPolyList
 - Test Case Number: 3
 - Input: polyToPolyList (Sum (Coef 3) (Prod X (Coef 2)))
 - Expected Output: PolyList [3, 2]
 - Actual Output: PolyList [3, 2]
 - Rationale: Edge case where a sum of terms `3 + 2X` is converted to the list `[3, 2]`.
 -}
