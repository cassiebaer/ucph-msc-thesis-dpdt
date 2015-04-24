module PureSensitivity
import Data.So
import Rational
%default total

Sensitivity : Type
Sensitivity = Rational

Budget : Type
Budget = Sensitivity

SensitiveFunction : Type
SensitiveFunction = Sensitivity -> Type -> Type -> Type

class Sensitive (f : SensitiveFunction) where
  arr : (a -> b) -> f s a b
  (.) : f s b c -> f s' a b -> f (s + s') a c
  app : f s a b -> (t:Budget) -> a -> { auto p : So (s <= t) } -> b

------------------------------------------------------------------------------
-- Flat
------------------------------------------------------------------------------

namespace Flat
  data Flat : SensitiveFunction where
    MkFlat : (a -> b) -> Flat s a b

  flatComp : Flat s b c -> Flat s' a b -> Flat (s + s') a c
  flatComp (MkFlat f) (MkFlat g) = MkFlat (f . g)

  flatApply : Flat s a b -> (t:Budget) -> a -> { auto p : So (s <= t) } -> b
  flatApply (MkFlat f) _ x = f x

  instance Sensitive Flat where
    arr = MkFlat
    (.) = flatComp
    app = flatApply

------------------------------------------------------------------------------
-- Tree
------------------------------------------------------------------------------

namespace Tree
  data Tree : SensitiveFunction where
    TreeLeaf : (a -> b) -> Tree s a b
    TreeNode : Tree s b c -> Tree s' a b -> Tree (s + s') a c 

  treeApply : Tree s a b -> (t:Budget) -> a -> { auto p : So (s <= t) } -> b
  treeApply f _ x = treeApply' f x
    where treeApply' : Tree s a b -> a -> b
          treeApply' (TreeLeaf f)   x = f x
          treeApply' (TreeNode f g) x = treeApply' f (treeApply' g x)

  instance Sensitive Tree where
    arr = TreeLeaf
    (.) = TreeNode
    app = treeApply

