module Query

import Attribute
import Debug.Error -- TODO: find a better way to deal with partiality of compose
import Expr

------------------------------------------------------------------------------
-- Type Aliases
------------------------------------------------------------------------------

TableName : Type
TableName = String
%name TableName tblName

Scheme : Type
Scheme = List Attribute
%name Scheme attrs

Assoc : Type
Assoc = List (Attribute,Attribute)
%name Assoc assoc

------------------------------------------------------------------------------
-- Abstract Syntax
------------------------------------------------------------------------------

data RelOp     = Times | Union | Intersect | Divide | Difference
%name RelOp rop

data PrimQuery = BaseTable TableName Scheme
               | Project   Assoc     PrimQuery
               | Restrict  PrimExpr  PrimQuery
               | Binary    RelOp     PrimQuery PrimQuery
               | Empty
%name PrimQuery q1,q2,q3

------------------------------------------------------------------------------
-- Utility Functions
------------------------------------------------------------------------------

scheme : PrimQuery -> Scheme
scheme Empty = []
scheme (BaseTable tblName attrs) = attrs
scheme (Project assoc q1) = map fst assoc
scheme (Restrict e1 q1) = scheme q1
scheme (Binary rop q1 q2) = case rop of
                                 Times      => attr1 ++ attr2
                                 Union      => attr1
                                 Intersect  => attr1 \\ attr2
                                 Divide     => attr1
                                 Difference => attr1
                               where 
                                 attr1 : Scheme
                                 attr1  = scheme q1
                                 attr2 : Scheme
                                 attr2  = scheme q2

assocFromScheme : Scheme -> Assoc
assocFromScheme scheme = map (\attr => (attr,attr)) scheme

compose : Assoc -> Assoc -> Assoc
compose assoc1 assoc2 = flip map assoc2 (\(a1,a2) => (a1, case lookup a2 assoc1 of
                                                               Just a3 => a3
                                                               Nothing => error "partial compose"))









