module ExprTests

import Database.PowerOfPi

testSchema : Schema
testSchema = ["id":::Int, "name":::String, "city":::String, "age":::Int]

testRow : Row testSchema
testRow = [1, "Knut", "Cph", 26]

testLit : Integer
testLit = evalExpr (Lit 5) testRow

exprLookup : evalExpr (testSchema ^ "id") testRow = 1
exprLookup = Refl

exprPlus : Int
exprPlus = evalExpr ((testSchema ^ "id") + (testSchema ^ "age")) testRow

nameAndCity : Row testSchema -> String
nameAndCity [id, name, city, age] = name ++ " and " ++ city

exprPureFn : Int
exprPureFn = evalExpr (PureFn (+10) (testSchema ^ "age")) testRow

exprRowFn : Row testSchema -> String
exprRowFn [id,name,city,age] = name ++ " in " ++ city

