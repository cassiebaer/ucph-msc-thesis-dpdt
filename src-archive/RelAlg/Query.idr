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

------------------------------------------------------------------------------
-- Monad Comprehensions
------------------------------------------------------------------------------

QueryState : Type
QueryState = (Int,PrimQuery)

record Query : Type -> Type where
  MkQuery : (runQuery : QueryState -> (a,QueryState)) -> Query a

-- TODO: Verify whether this is the right instance to use
instance Functor Query where
    map f (MkQuery m) = MkQuery $ \r => let (x,r') = m r
                                         in (f x,r')

-- TODO: Verify whether this is the right instance to use
instance Applicative Query where
    pure x = MkQuery $ \r => (x,r)
    (<*>) (MkQuery f) (MkQuery g) = MkQuery $ \r => let (k,r1) = f r
                                                        (x,r2) = g r1
                                                     in (k x,r2)

instance Monad Query where
    (MkQuery m) >>= k = MkQuery $ \r => let (x,r')      = m r
                                            (MkQuery f) = k x
                                         in f r'

updatePrimQuery : (PrimQuery -> PrimQuery) -> Query PrimQuery
updatePrimQuery f = MkQuery $ \(i,qt) => (qt, (i,f qt) )

unique : Query Int
unique = MkQuery $ \(i,qt) => (i, (i+1,qt) )

data Rel = MkRel Assoc
data Attr = MkAttr Attribute
data Table = MkTable TableName Scheme

infixl 9 !!
(!!) : Rel -> Attr -> Expr a
(MkRel assoc) !! (MkAttr attr) = case lookup attr assoc of
                                      Just realname => MkExpr (AttrExpr realname)
                                      Nothing       => error $ "unknown attribute" ++ show attr

uniqueAssoc : Scheme -> Query Assoc
uniqueAssoc scheme = do
    i <- unique
    return $ map (\attr => (attr ++ show i, attr)) scheme

inverse : List (a,b) -> List (b,a)
inverse = map (\(x,y) => (y,x))

table : Table -> Query Rel
table (MkTable name scheme) = do
    assoc <- uniqueAssoc scheme
    updatePrimQuery $ \q => Binary Times q (Project assoc (BaseTable name scheme))
    return $ MkRel (inverse assoc)

namespace Test

  foo : Query (Expr a)
  foo = do
      t <- table $ MkTable "toons" ["name","age"]
      return $ t !! MkAttr "name"

  bar : (Expr a,QueryState)
  bar = (runQuery foo) (1,BaseTable "toons" ["name","age"])
















