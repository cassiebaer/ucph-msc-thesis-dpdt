module Arith
%default total

data Term : Type where
  -- Bool --
  TmTrue   : Term
  TmFalse  : Term
  TmIf     : Term -> Term -> Term -> Term
  -- Nat --
  TmZero   : Term
  TmSucc   : Term -> Term
  TmPred   : Term -> Term
  TmIsZero : Term -> Term
%name Term t1,t2,t3

isNum : Term -> Bool
isNum TmZero      = True
isNum (TmSucc t1) = isNum t1
isNum (TmPred t1) = isNum t1
isNum _           = False

eval1 : Term -> Maybe Term
-- Bool --
eval1 (TmIf TmTrue  t2 _) = return t2                       -- E-IfTrue
eval1 (TmIf TmFalse _ t3) = return t3                       -- E-IfFalse
eval1 (TmIf t1 t2 t3)     = return (TmIf !(eval1 t1) t2 t3) -- E-If
-- Nat --
eval1 (TmSucc t1)         = return (TmSucc !(eval1 t1))     -- E-Succ
eval1 (TmPred TmZero)     = return TmZero                   -- E-PredZero
eval1 (TmPred (TmSucc t1)) with (isNum t1)                  -- E-PredSucc
  | True  = return t1
  | False = Nothing
eval1 (TmPred t1)         = return (TmPred !(eval1 t1))     -- E-Pred
eval1 (TmIsZero TmZero)   = return TmTrue                   -- E-IsZeroZero
eval1 (TmIsZero (TmSucc t1)) with (isNum t1)                -- E-IsZeroSucc
  | True  = return TmFalse
  | False = Nothing
eval1 (TmIsZero t1)       = return (TmIsZero !(eval1 t1))   -- E-IsZero
eval1 _ = Nothing

%assert_total -- why can't I assert_smaller on the recursive case?
eval : Term -> Term
eval t1 = case eval1 t1 of
               Just t1' => eval t1'
               Nothing  => t1

-- Combine functions and syntax extensions to provide a usable language interface

true : Term
true = TmTrue

false : Term
false = TmFalse

syntax "|" [t1] "," [p] "|" [t2] "," "otherwise" = TmIf p t1 t2

z : Term
z = TmZero

s : Term -> Term
s = TmSucc

p : Term -> Term
p = TmPred

syntax "isZ" [n] = TmIsZero n

-- Example Function using special TmIf syntax

partial
isOneOrZero : Term -> Term
isOneOrZero n = | TmTrue  , isZ (p n)
                | TmFalse , otherwise

foo : Term
foo = eval $ isOneOrZero (s z) -- should evaluate to TmTrue

