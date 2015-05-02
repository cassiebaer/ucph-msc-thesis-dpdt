module ExprTests

import Database.PowerOfPi

testSchema : Schema
testSchema = ["id":::Int, "name":::String, "city":::String, "age":::Int]

testRow : Row testSchema
testRow = [1, "Knut", "Cph", 26]

exprLookup : evalExpr (testSchema ^ "id") testRow = 1
exprLookup = Refl

exprPlus : Int
exprPlus = evalExpr ((testSchema ^ "id") + (testSchema ^ "age")) testRow
