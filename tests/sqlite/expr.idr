module ExprTests

import Database.PowerOfPi
import Database.Backend.SQLite.Expr

testSchema : Schema
testSchema = ["id":::Nat, "name":::String, "city":::String, "age":::Nat]

five : Expr testSchema Nat
five = Lit 5

testLit : eval five = "5"
testLit = Refl

exprLookup : eval (testSchema ^ "id") = "id"
exprLookup = Refl

exprPlus : eval ((testSchema ^ "id") + five) = "id + 5"
exprPlus =  Refl
