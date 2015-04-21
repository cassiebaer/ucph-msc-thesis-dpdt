
import Data.So

-----------------------------------------------------------------
-- Basic Function: (a -> b)
----------------------------------------------------------------

-- Define our Arrow type to represent functions
data Arrow : Type -> Type -> Type where
  Fn : (a -> b) -> Arrow a b

-- Give a way to apply our functions
total
fn_apply : Arrow a b -> a -> b
fn_apply (Fn f) x = f x

-- Examples of basic Fn application
namespace fn_apply_examples
  foo1 : Int
  foo1 = fn_apply (Fn (2*)) 21

----------------------------------------------------------------
-- c-Sensitive Function: (a ->c b)
----------------------------------------------------------------

-- We represent Sensitivity as Maybe Float where Nothing == Infinity

-- foo combines function sensitivities under composition
foo : Maybe Float -> Maybe Float -> Maybe Float
foo Nothing _ = Nothing
foo _ Nothing = Nothing
foo (Just x) (Just y) = Just (x + y)

data Arrow' : Maybe Float -> Type -> Type -> Type where
  FnSens : (a -> b) -> (x:Maybe Float) -> Arrow' x a b
  FnComp : Arrow' s a b -> Arrow' s' b c -> Arrow' (foo s s') a c

maybLT : Maybe Float -> Maybe Float -> Bool
maybLT Nothing Nothing = True
maybLT Nothing (Just z) = False
maybLT (Just x) Nothing = True
maybLT (Just x) (Just z) = x < z

-- Then we expect that a budget can be specified like so:
main' : Arrow' mayb db r -> So (maybLT mayb budget) -> r

-- Biggest question: function sensitivities are given by the user
-- Can we use Idris to prove that a function is 1-sensitive?
-- Problably.

