------------------------------------------------------------------------------
-- Step 1: Abstract Syntax
------------------------------------------------------------------------------

data BinOp = OpEq | OpLt | OpLtEq
%name BinOp f

data UnOp = OpNot | OpAsc | OpDesc | OpIsNull | OpIsNotNull
%name UnOp f

data AggrOp = AggrCount | AggrSum | AggrAvg | AggrStdDev
%name AggrOp f

data Attribute

data PrimExpr = AttrExpr Attribute
              | BinExpr BinOp PrimExpr PrimExpr
              | UnExpr UnOp PrimExpr
              | AggrExpr AggrOp PrimExpr
              | ConstExpr String
%name PrimExpr e1,e2,e3

instance Show BinOp where
    show OpEq = "="
    show OpLt = "<"
    show OpLtEq = "<="

instance Show UnOp where
    show OpNot = "!"
    show OpAsc = "asc"
    show OpDesc = "desc"
    show OpIsNull = "null"
    show OpIsNotNull = "!null"

instance Show AggrOp where
    show AggrCount = "count"
    show AggrSum = "sum"
    show AggrAvg = "avg"
    show AggrStdDev = "stddev"

parens : String -> String
parens x = "(" ++ x ++ ")"

instance Show PrimExpr where
    show (AttrExpr x) = ?showPrimExpr_AttrExpr
    show (BinExpr f e1 e2) = parens (show e1) ++ show f ++ parens (show e2)
    show (UnExpr f e1) = show f ++ parens (show e1)
    show (AggrExpr f e1) = show f ++ parens (show e1)
    show (ConstExpr x) = x
