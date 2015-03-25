{-
This is an implementation of Edwin Brady's "Well-typed Interpreter" from the Idris programming tutorial.
-}
module EB_WellTypedInterp

import Data.Fin
import Data.Vect

data Ty = TyInt | TyBool | TyFun Ty Ty

interpTy : Ty -> Type
interpTy TyInt = Int
interpTy TyBool = Bool
interpTy (TyFun x y) = interpTy x -> interpTy y

using (G : Vect n Ty)
  -- Variables are represented by a proof of their membership in the context
  data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
    Stop : HasType FZ (t :: G) t
    Pop  : HasType k G t -> HasType (FS k) (u :: G) t

  data Expr : Vect n Ty -> Ty -> Type where
    Var : HasType i G t -> Expr G t
    Val : (x:Int) -> Expr G TyInt
    Lam : Expr (a :: G) t -> Expr G (TyFun a t)
    App : Expr G (TyFun a t) -> Expr G a -> Expr G t
    Op  : (interpTy a -> interpTy b -> interpTy c)
       -> (Expr G a) -> (Expr G b) -> (Expr G c)
    If  : Expr G TyBool -> Lazy (Expr G a) -> Lazy (Expr G a) -> Expr G a

  data Env : Vect n Ty -> Type where
    Nil  : Env Nil
    (::) : interpTy a -> Env G -> Env (a :: G)

  lookup : HasType i G t -> Env G -> interpTy t
  lookup Stop    (x :: xs) = x
  lookup (Pop k) (x :: xs) = lookup k xs

  interp : Env G -> Expr G t -> interpTy t
  interp env (Var i) = lookup i env
  interp env (Val x) = x
  interp env (Lam abs) = \x => interp (x::env) abs
  interp env (App f x) = interp env f (interp env x)
  interp env (Op f x y) = f (interp env x) (interp env y)
  interp env (If p t f) = if interp env p then interp env t
                                          else interp env f


