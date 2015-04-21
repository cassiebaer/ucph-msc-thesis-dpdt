module Arith

data Ty = INT | BOOL

convert : Ty -> Type
convert INT = Int
convert BOOL = Bool

data Term : Ty -> Type where
          TRUE  : Term BOOL
          FALSE : Term BOOL
          ZERO  : Term INT
          SUCC  : Term INT -> Term INT
          PRED  : Term INT -> Term INT
          IF_ELSE : Term BOOL -> 
                    Term ty -> 
                    Term ty ->
                    Term ty
          IS_ZERO : Term INT -> Term BOOL
          

eval : Term ty -> convert ty
eval TRUE             = True
eval FALSE            = False
eval ZERO             = 0
eval (SUCC t)         = (eval t) + 1
eval (PRED t)         = (eval t) - 1
eval (IF_ELSE p t t') = if eval p then eval t else eval t'
eval (IS_ZERO t)      = if (eval t == 0) then True else False

