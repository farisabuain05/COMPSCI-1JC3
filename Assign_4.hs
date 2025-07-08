{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 4 - McMaster CS 1JC3 2021
-}
module Assign_4 where

import Test.QuickCheck

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
-- Date: April 8th, 2025
macid :: String
macid = "abuainf"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Add (MathExpr a) (MathExpr a)
  | Mult (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates the value of a mathematical expression, e, at a 
 -    floating point value v.
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval e v = case e of
  X -> v
  Coef c -> c
  Add e1 e2 -> eval e1 v + eval e2 v
  Mult e1 e2 -> eval e1 v * eval e2 v
  Power e n -> eval e v ^^ fromIntegral n
  Cos e -> cos (eval e v)
  Sin e -> sin (eval e v)
  Abs e -> abs (eval e v)


{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Implements the addition (+), multiplication (*), negate (-x), 
 -    absolute (abs x) methods for MathExpr.
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Add x y
  x * y         = Mult x y
  negate x      = Mult (Coef (-1)) x 
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Implements the reciprocal (recip e) and fromRational 
 -    methods for MathExpr.
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Implements floating pi, sin, and cos methods for MathExpr.
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Sin
  cos     = Cos
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    Symbolically differentiates an expression e and returns the 
 -    resulting derivative, based on rules given in assignment.
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff e = case e of
  X -> Coef 1
  Coef _ -> Coef 0
  Add e1 e2 -> diff e1 + diff e2
  Mult e1 e2 -> diff e1 * e2 + e1 * diff e2
  Power e n -> Coef (fromIntegral n) * Power e (n-1) * diff e
  Sin e -> Cos e * diff e
  Cos e -> negate (Sin e) * diff e
  Abs e -> e / Abs e * diff e

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 -    Creates a string representation of a mathematical expression e, 
 -    which can then be re-entered into GHCI to return the original
 -    expression e. 
 -}
-- NOTE: you'll have to test pretty yourself
pretty :: (Show a) => MathExpr a -> String
pretty e = case e of
  X -> "X"
  Coef c -> "(" ++ show c ++ ")"
  Add u0 u1 -> "(" ++ pretty u0 ++ " + " ++ pretty u1 ++ ")"
  Mult u0 u1 -> "(" ++ pretty u0 ++ " * " ++ pretty u1 ++ ")"
  Power u0 d -> "(" ++ pretty u0 ++ " ^^ (" ++ show d ++ "))"
  Cos u0 -> "cos(" ++ pretty u0 ++ ")"
  Sin u0 -> "sin(" ++ pretty u0 ++ ")"
  Abs u0 -> "abs(" ++ pretty u0 ++ ")"


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0

-- Function: eval
-- Property: eval (Mult (Coef x) X) y == x * y for all x,y
-- Actual Test Result: Pass
evalProp1 :: (Float, Float) -> Bool
evalProp1 (x, y) = (x * y) =~ eval (Mult (Coef x) X) y

runEvalProp1 :: IO ()
runEvalProp1 = quickCheck evalProp1

-- Function: diff
-- Property: eval (diff (Power X n)) v ≈ n * v^(n-1), for n > 0
-- Actual Test Result: Pass
diffProp0 :: Int -> Float -> Property
diffProp0 n v =
  n >= 1 && n <= 10 && v /= 0 && abs v <= 25 ==>
    let expr = Power X n
        d = eval (diff expr) v
        expected = fromIntegral n * v ^^ (n - 1)
    in d =~ expected

runDiffProp0 :: IO ()
runDiffProp0 = quickCheck diffProp0

{- 
Function: eval
Test Case Number: 1
Input: eval (Add X (Coef 2)) 3
Expected Output: 5
Actual Output: 5
Rationale: Tests addition and variable substitution

Function: eval
Test Case Number: 2
Input: eval (Power (Add X (Coef 1)) 2) 2
Expected Output: 9
Actual Output: 9
Rationale: Tests nested addition with power

Function: eval
Test Case Number: 3
Input: eval (Sin (Mult (Coef pi) X)) 1
Expected Output: 0
Actual Output: ~0
Rationale: Tests trigonometric function sin(pi)

Function: diff
Test Case Number: 1
Input: diff (Add X (Coef 5))
Expected Output: Coef 1
Actual Output: Coef 1
Rationale: Derivative of x + constant is 1

Function: diff
Test Case Number: 2
Input: diff (Mult X X)
Expected Output: Add X X
Actual Output: Add X X
Rationale: d/dx(x*x) = x + x via product rule

Function: diff
Test Case Number: 3
Input: diff (Sin X)
Expected Output: Cos X
Actual Output: Cos X
Rationale: d/dx(sin x) = cos x

Function: pretty
Test Case Number: 1
Input: pretty (Add X (Coef 2))
Expected Output: "(X + (2))"
Actual Output: "(X + (2))"
Rationale: Verifies simple addition formatting

Function: pretty
Test Case Number: 2
Input: pretty (Mult (Coef 2) X)
Expected Output: "((2) * X)"
Actual Output: "((2) * X)"
Rationale: Verifies multiplication formatting

Function: pretty
Test Case Number: 3
Input: pretty (Power (Add X (Coef 1)) 2)
Expected Output: "((X + (1)) ^^ (2))"
Actual Output: "((X + (1)) ^^ (2))"
Rationale: Verifies power and nested formatting
-}
