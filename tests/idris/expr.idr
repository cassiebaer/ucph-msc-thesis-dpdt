module ExprTests

import Database.PowerOfPi.Idris

testSchema : Schema
testSchema = ["id":::Int, "name":::String, "city":::String, "age":::Int]

testRow : Row testSchema
testRow = [1, "Knut", "Cph", 26]

testLit : Integer
testLit = eval (Lit 5) testRow

exprLookup : eval (testSchema ^ "id") testRow = 1
exprLookup = Refl

exprPlus : Int
exprPlus = eval ((testSchema ^ "id") + (testSchema ^ "age")) testRow

nameAndCity : Row testSchema -> String
nameAndCity [id, name, city, age] = name ++ " and " ++ city

exprPureFn : Int
exprPureFn = eval (PureFn (+10) (testSchema ^ "age")) testRow

