module Basic001

import Database.PowerOfPi.SQLite

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Nat ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : Query SQLite Person
people = Table "People"

foods : Query SQLite Food
foods = Table "Foods"

namespace Union

  unionPeopleWithItself : eval (people `Union` people) = "(People) Union (People)"
  unionPeopleWithItself = Refl

  unionPeopleWithNew : eval (people `Union` (Table "P2")) = "(People) Union (P2)"
  unionPeopleWithNew = Refl

namespace Diff

  diffPeopleWithItself : eval (people `Diff` people) = "(People) Intersect (People)"
  diffPeopleWithItself = Refl
  
  diffPeopleWithNew : eval (people `Diff` (Table "P2")) = "(People) Intersect (P2)"
  diffPeopleWithNew = Refl

namespace Product
  fooTable : Query SQLite ["Foo":::Char]
  fooTable = Table "Bar"

  prodPeopleWithABC : eval (Product people fooTable) = "(People) , (Bar)"
  prodPeopleWithABC = Refl

namespace Projection
  fooProj : String -> Maybe String
  fooProj "Name" = Just "FirstName"
  fooProj _      = Nothing

  projPeopleFirstNames : eval (Projection fooProj people) = "Select FirstName From (People)"
  projPeopleFirstNames = Refl

namespace Select
  expr : Expr Person Bool
  expr = (Person ^ "Age") == (Lit 25)

  selectOneTable : eval (Select expr people) = "Select Name, Age From People Where Age == 25"
  selectOneTable = Refl

--namespace Aggregation
--
--  countTable : eval (Aggregation people (+) (the Nat 0) (Lit 1)) = length (eval people)
--  countTable = Refl
--
--  sumAges : eval (Aggregation people (+) (the Nat 0) (Person^"Age")) = 53
--  sumAges = Refl
--
