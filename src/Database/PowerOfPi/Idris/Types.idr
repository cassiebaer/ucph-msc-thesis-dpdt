module Database.PowerOfPi.Idris.Types

import Database.PowerOfPi.Abstract

||| Represents a row in a table.
||| We impose an Eq constraint for the Expr language.
data Row : Schema -> Type where
  Nil  : Row []
  (::) : Eq t => t -> Row s -> Row (name:::t::s)

(++) : Row s -> Row s' -> (Row (s ++ s'))
(++) []      ys = ys
(++) (x::xs) ys = x :: xs ++ ys

instance Eq (Row s) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

project : (f:String -> Maybe String) -> Row s -> Row (projectedSchema f s)
project f []      {s=[]}        = []
project f (r::rs) {s=n:::t::as} with (f n)
  project f (r::rs) {s=n:::t::as} | Nothing = project f rs
  project f (r::rs) {s=n:::t::as} | Just n' = r :: project f rs

getSchema : List $ Row s -> Schema
getSchema {s} _ = s

