module Main
import Control.Monad.Identity
import Control.Monad.State
%default total

{-
   This is an exploration of indexing Query types by their schema
   e.g.
        project ["name"] $ QueryExpr ["name","age"]
        => QueryExpr ["name"]
   and the relational algebra given such a type system.
-}

Schema : Type
Schema = List String

mutual
  ||| Represents binary operations over relations
  data BinRel : Schema -> Type where
       Union        : QueryExpr s -> QueryExpr s -> BinRel s
       Intersection : QueryExpr s -> QueryExpr s -> BinRel s
       Difference   : QueryExpr s -> QueryExpr s -> BinRel s
       Join         : QueryExpr s -> QueryExpr t -> BinRel (s++t)

  ||| Represents a query expression, indexed over its current schema
  data QueryExpr : Schema -> Type where
       Empty       : QueryExpr Nil
       BaseTable   : String -> (s:Schema) -> QueryExpr s
       Projection  : (s:Schema) -> QueryExpr t -> QueryExpr s
       Selection   : (Bool -> Bool) -> QueryExpr s -> QueryExpr s
       Rename      : (s:Schema) -> QueryExpr t -> QueryExpr s
       Binary      : BinRel s    -> QueryExpr s

------------------------------------------------------------------------------
-- Embedding into Idris
------------------------------------------------------------------------------

table : String -> (s:List String) -> QueryExpr s
table name schema = BaseTable name schema

project : (s:List String) -> QueryExpr t -> QueryExpr s
project schema expr = Projection schema expr

select : (Bool -> Bool) -> QueryExpr s -> QueryExpr s
select p expr = Selection p expr 

join : QueryExpr s -> QueryExpr t -> QueryExpr (s++t)
join t1 t2 = Binary (Join t1 t2)

-- and more..

------------------------------------------------------------------------------
-- Show Instances
------------------------------------------------------------------------------

instance Show (BinRel s) where
    show (Union x y) = "Union"
    show (Intersection x y) = "Intersection"
    show (Difference x y) = "Difference"
    show (Join x y) = "Join"

instance Show (QueryExpr s) where
    show Empty = "Empty"
    show (BaseTable x s) = "BaseTable: " ++ x ++ (show s)
    show (Projection s x) = "Projection: " ++ (show s)
    show (Selection f x) = "Selection: (ERR_NOT_YET_IMPL)"
    show (Rename s x) = "Rename: " ++ (show s)
    show (Binary x) = show x

------------------------------------------------------------------------------
-- GraphViz
------------------------------------------------------------------------------

data GVState = GVSt Int (List String) (List (Int,Int))

addDecl : (Show a) => a -> State GVState Int
addDecl x = do
    (GVSt i d a) <- get
    put (GVSt (i+1) (d++[show i ++ "[label=\"" ++ show x ++ "\"]"]) a)
    return i

addArr : Int -> Int -> State GVState ()
addArr i j = modify $ \(GVSt i' decl arr) => GVSt i' decl (arr++[(i,j)])

partial
_toGV : QueryExpr s -> State GVState Int

partial
addChild : QueryExpr s -> QueryExpr t -> State GVState Int
addChild x y = do
    i <- addDecl x
    j <- _toGV y
    addArr i j
    (GVSt _ _ a) <- get
    return i

partial
addBinChild : BinRel s -> QueryExpr t -> QueryExpr u -> State GVState Int
addBinChild x y z = do
    i <- addDecl x
    j <- _toGV y
    k <- _toGV z
    addArr i j
    addArr i k
    return i

partial
_toGVB : BinRel s -> State GVState Int
_toGVB q@(Union x y) = addBinChild q x y
_toGVB q@(Intersection x y) = addBinChild q x y
_toGVB q@(Difference x y) = addBinChild q x y
_toGVB q@(Join x y) = addBinChild q x y

_toGV q@(Projection s x) = addChild q x
_toGV q@(Selection f x) = addChild q x
_toGV q@(Rename s x) = addChild q x
_toGV q@(Binary x) = _toGVB x
_toGV q@(Empty) = addDecl q
_toGV q@(BaseTable x s) = addDecl q

execState : State s a -> s -> s
execState m st = snd $ runIdentity (runStateT m st)

partial
toGV : QueryExpr s -> String
toGV x = let (GVSt _ d a) = execState (_toGV x) (GVSt 0 [] []) 
          in concat $ intersperse "\n" d ++ ["\n"]
             ++ intersperse "\n" (map (\(i,j) => show i ++ " -> " ++ show j) a)

------------------------------------------------------------------------------
-- Example
------------------------------------------------------------------------------

-- toGV foo ==> http://i61.tinypic.com/2ibohs2.png
foo : QueryExpr ["age","salary"]
foo = let t1 = table "Ages" ["name","age"]
          t2 = table "Salaries" ["name","salary"]
       in project ["age","salary"] (join t1 (select id t2))

