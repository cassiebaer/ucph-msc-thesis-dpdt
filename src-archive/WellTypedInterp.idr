-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.39.2895&rep=rep1&type=pdf

module WellTypedInterp
%default total

-- Our base types
data Ty = TBool | TInt | TArrow Ty Ty

InterpTy : Ty -> Type
InterpTy TBool = Bool
InterpTy TInt  = Int
InterpTy (TArrow a b) = InterpTy a -> InterpTy b

-- Symbol is just a type synonym for String
Symbol : Type
Symbol = String

-- Our base expressions
data Expr = EBool Bool | EInt Int
          | EAdd Expr Expr | EAnd Expr Expr | ELE Expr Expr
          | EVar Symbol | ELam Ty Symbol Expr | EAp Expr Expr

-- ADT representing a typing proof of `a` and `b`
data (^) a b = (&) a b
infixl 5 ^
infixl 5 &

TEnv : Type
TEnv = Symbol -> Ty
extendT : TEnv -> Symbol -> Ty -> TEnv

-- HasType captures our typing rules directly
-- We use the Unit type for truthiness and the empty type otherwise
HasType : TEnv -> Expr -> Ty -> Type
HasType g (EBool _)    (TBool) = ()
HasType g (EInt  _)    (TInt)  = ()
HasType g (EAdd e1 e2) (TInt)  = HasType g e1 TInt  ^ HasType g e2 TInt
HasType g (EAnd e1 e2) (TBool) = HasType g e1 TBool ^ HasType g e2 TBool
HasType g (ELE  e1 e2) (TBool) = HasType g e1 TInt  ^ HasType g e2 TInt
HasType g (EVar x)      t      = (g x) = t
HasType g (EAp f a)     t      = (s ** HasType g f (TArrow s t) ^ HasType g a s)
HasType g (ELam s x e1) (TArrow s' t) = HasType (extendT g x s) e1 t ^ (s = s')
HasType _ _             _      = Void

-- Our well-typed interpreter
--interp : (e:Expr) -> (t:Ty) -> HasType e t -> InterpTy t
--interp (EBool b)    TBool p = b
--interp (EInt  i)    TInt  p = i
--interp (EAdd e1 e2) TInt  (p1 & p2) = interp e1 TInt  p1 +  interp e2 TInt  p2
--interp (EAnd e1 e2) TBool (p1 & p2) = interp e1 TBool p1 && interp e2 TBool p2
--interp (ELE  e1 e2) TBool (p1 & p2) = interp e1 TInt  p1 <= interp e2 TInt  p2

-- TODO:
-- - Implement `interp` cases for:
--   * EVar
--   * ELam
--   * EAp
-- - Implement environment (TEnv)
--   * Section 4.2 in the paper
--   * Maybe we should, as an exercise, rewrite the environment to use deBruijn indices

