module Expr

------------------------------------------------------------------------------
-- Abstract Syntax
------------------------------------------------------------------------------

data Attribute

||| An enumeration of all binary operators
data BinOp    = OpEq | OpLt | OpLtEq
           -- | ...
%name BinOp bop

||| An enumeration of all unary operators
data UnOp     = OpNot | OpAsc | OpDesc | OpIsNull | OpIsNotNull
           -- | ...
%name UnOp uop

||| An enumeration of all aggregration operators
data AggrOp   = AggrCount | AggrSum | AggrAvg | AggrStdDev
           -- | ...
%name AggrOp aop

||| Represents an (untyped) abstract syntax tree for our query language
|||
||| e.g. `"1 = 3" <=> BinExpr OpEq (ConstExpr (show 1)) (ConstExpr (show 3))`
data PrimExpr = AttrExpr Attribute
              | BinExpr  BinOp     PrimExpr PrimExpr
              | UnExpr   UnOp      PrimExpr
              | AggrExpr AggrOp    PrimExpr
              | ConstExpr String
%name PrimExpr e1,e2,e3

------------------------------------------------------------------------------
-- Code Generator
------------------------------------------------------------------------------

-- We plan on only having one concrete syntax, so we might as well use `Show`.

instance Show BinOp where
    show OpEq = "="
    show OpLt = "<"
    show OpLtEq = "<="

instance Show UnOp where
    show OpNot = "not"
    show OpAsc = "asc"
    show OpDesc = "desc"
    show OpIsNull = "isNull"
    show OpIsNotNull = "isNotNull"

instance Show AggrOp where
    show AggrCount = "count"
    show AggrSum = "sum"
    show AggrAvg = "avg"
    show AggrStdDev = "stdDev"

parens : String -> String
parens x = "(" ++ x ++ ")"

instance Show PrimExpr where
    show (AttrExpr x) = ?showAttribute
    show (BinExpr bop e1 e2) = parens (show e1) ++ show bop ++ parens (show e2)
    show (UnExpr uop e1) = show uop ++ parens (show e1)
    show (AggrExpr aop e1) = show aop ++ parens (show e1)
    show (ConstExpr x) = x

------------------------------------------------------------------------------
-- Type Embedding
------------------------------------------------------------------------------

data Expr a = MkExpr PrimExpr

constant : Show a => a -> Expr a
constant x = MkExpr (ConstExpr (show x))

-- Binary Operations

infixl 5 .==.
(.==.) : (Eq a) => Expr a -> Expr a -> Expr Bool
(.==.) (MkExpr e1) (MkExpr e2) = MkExpr (BinExpr OpEq e1 e2)

infixl 8 .<.
(.<.) : Expr Int -> Expr Int -> Expr Int
(.<.) (MkExpr e1) (MkExpr e2) = MkExpr (BinExpr OpLt e1 e2)

infixl 6 .<=.
(.<=.) : Expr Int -> Expr Int -> Expr Bool
(.<=.) (MkExpr e1) (MkExpr e2) = MkExpr (BinExpr OpLtEq e1 e2)

