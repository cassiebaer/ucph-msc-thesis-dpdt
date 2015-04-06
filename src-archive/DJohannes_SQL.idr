------------------------------------------------------------------------------
-- Step 1: Abstract Syntax
------------------------------------------------------------------------------

data BinOp = OpEq | OpLt | OpLtEq | OpPlus
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
    show OpPlus = ".+."

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

------------------------------------------------------------------------------
-- Step 2: Abstract Syntax Embedding
------------------------------------------------------------------------------

namespace untyped

  constant : (Show a) => a -> PrimExpr
  constant x = ConstExpr (show x)

  infixl 8 .+.
  (.+.) : PrimExpr -> PrimExpr -> PrimExpr
  (.+.) x y = BinExpr OpPlus x y

  sum : Int -> PrimExpr
  sum n = if (n <= 0) then constant 0 else constant n .+. sum (n-1)

------------------------------------------------------------------------------
-- Step 3: Type Embedding
------------------------------------------------------------------------------

data Expr a = MkExpr PrimExpr

constant : (Show a) => a -> Expr a
--constant x = MkExpr $ ConstExpr (show x)
constant = MkExpr . untyped.constant

