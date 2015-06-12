module Database.Backend.Idris.Query

import Database.PowerOfPi.Query
import Database.Backend.Idris.Row
import Database.Backend.Idris.Expr
import Database.Backend.Backend
%default total

namespace Query
 
  put : Eq k => k -> Row s -> List $ Row ["k":::k, "v":::List (Row s)] -> List $ Row ["k":::k, "v":::List (Row s)]
  put k r []              = [[k, [r]]]
  put k r ([k', rs] :: ks) = if k == k' then [ k', (r::rs) ] :: ks
                                        else [ k , rs ] :: put k r ks
                                        
  --Essentially fold TODO: Use fold
  constructMap : Eq k => Expr s k -> List (Row s) -> List $ Row ["k":::k, "v":::List (Row s)]
  constructMap e []      = []
  constructMap e (r::rs) = put (eval e r) r $ constructMap e rs
  
  -- I want this function to transform grouping schemas' "v" type from Query b s to List $ Row s
  -- in order to get the correct return type of eval (GroupBy _ _)
  foo : Schema -> Schema
  foo ["k":::k, "v":::(Query b s)] = ["k":::k, "v":::List (Row s)]
  foo s = s

  bar : Schema
  bar = ["k":::Nat, "v":::Query Idris ["Age":::Nat]]

--  ||| Evaluates a Query, returning a List of Rows.
--  %assert_total
--  eval : Query Idris s -> List $ Row $ foo s
--  eval (Table xs)       = xs
--  eval (Union x y)      = (eval x) ++ (eval y)
--  eval (Diff x y)       = (eval x) \\ (eval y)
--  eval (Product x y)    = [ x' ++ y' | x' <- eval x, y' <- eval y ]
--  eval (Projection f x) = map (project f) (eval x)
--  eval (Select e x)     = filter (eval e) (eval x)
--  eval (GroupBy e x)    = constructMap e $ eval x


--  %assert_total  --TODO: make total
--  eval : Query Idris (Grouping k s) -> List (k, List (Row s))
--  eval (GroupBy e x) = constructMap e $ eval x
