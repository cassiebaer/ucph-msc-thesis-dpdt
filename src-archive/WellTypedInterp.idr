module WellTypedInterp

%default total

-- Our base types
data Ty = TBool | TInt

InterpTy : Ty -> Type
InterpTy TBool = Bool
InterpTy TInt  = Int

-- Our base expressions
data Expr = EBool Bool | EInt Int
          | EAdd Expr Expr | EAnd Expr Expr | ELE Expr Expr

-- ADT representing a typing proof of (a ^ b)
data (^) a b = (&) a b
infixl 5 ^
infixl 5 &

-- HasType captures our typing rules directly
-- We use the Unit type for truthiness and the empty type otherwise
HasType : Expr -> Ty -> Type
HasType (EBool _)    (TBool) = ()
HasType (EInt  _)    (TInt)  = ()
HasType (EAdd e1 e2) (TInt)  = HasType e1 TInt  ^ HasType e2 TInt
HasType (EAnd e1 e2) (TBool) = HasType e1 TBool ^ HasType e2 TBool
HasType (ELE  e1 e2) (TBool)  = HasType e1 TInt  ^ HasType e2 TInt
HasType _            _       = Void

-- Our well-typed interpreter
interp : (e:Expr) -> (t:Ty) -> HasType e t -> InterpTy t
interp (EBool b)    TBool p = b
interp (EInt  i)    TInt  p = i
interp (EAdd e1 e2) TInt  (p1 & p2) = interp e1 TInt  p1 +  interp e2 TInt  p2
interp (EAnd e1 e2) TBool (p1 & p2) = interp e1 TBool p1 && interp e2 TBool p2
interp (ELE  e1 e2) TBool (p1 & p2) = interp e1 TInt  p1 <= interp e2 TInt  p2

