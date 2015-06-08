module Basic001

import Database.PowerOfPi
import Database.Backend.Backend
import Database.Backend.Idris.Row
import Database.Backend.Idris.Query

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Nat ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : Query Idris Person
people = Table [ [ "Casper" , 25 ]
               , [ "Knut"   , 26 ]
               , [ "Gismo"  ,  2 ]
               ]

foods : Query Idris Food
foods = Table [ [ "Casper" , "Bruschetta" ]
              , [ "Knut"   , "Prim"       ]
              , [ "Gismo"  , "Dog food"   ]
              ]

namespace Union

  unionPeopleWithItself : List (Row Person)
  unionPeopleWithItself = eval (people `Union` people)

  lengthUnionPeopleWithItself : length unionPeopleWithItself = 6
  lengthUnionPeopleWithItself = Refl

  unionPeopleWithNew : List (Row Person)
  unionPeopleWithNew = eval (people `Union` (Table [["Alice",18]]))

  lengthUnionPeopleWithNew : length unionPeopleWithNew = 4
  lengthUnionPeopleWithNew = Refl

namespace Diff

  diffPeopleWithItself : List (Row Person)
  diffPeopleWithItself = eval (people `Diff` people)

  lengthDiffPeopleWithItself : length diffPeopleWithItself = 0
  lengthDiffPeopleWithItself = Refl

  diffPeopleWithNew : List (Row Person)
  diffPeopleWithNew = eval (people `Diff` (Table [["Gismo",2]]))

  lengthDiffPeopleWithNew : length diffPeopleWithNew = 2
  lengthDiffPeopleWithNew = Refl

namespace Product

  prodPeopleWithABC : List (Row (Person ++ ["Foo":::Char]))
  prodPeopleWithABC = eval (Product people fooTable)
    where fooTable : Query Idris ["Foo":::Char]
          fooTable = Table [ [ 'A' ] , [ 'B' ] , [ 'C' ] ]

  lengthProdPeopleWithABC : length prodPeopleWithABC = 9
  lengthProdPeopleWithABC = Refl

namespace Projection

  projPeopleFirstNames : List (Row ["FirstName":::String])
  projPeopleFirstNames = eval (Projection fooProj people)
    where fooProj : String -> Maybe String
          fooProj "Name" = Just "FirstName"
          fooProj _      = Nothing

namespace Select

  selectOneTable : List (Row Person)
  selectOneTable = eval (Select expr people)
    where expr : Expr Person Bool
          expr = (Person ^ "Age") == (Lit 25)

 lengthSelectOneTable : length selectOneTable = 1
 lengthSelectOneTable = Refl

namespace Aggregation

  countTable : eval (Aggregation people (+) (the Nat 0) (Lit 1)) = length (eval people)
  countTable = Refl

  sumAges : eval (Aggregation people (+) (the Nat 0) (Person^"Age")) = 53
  sumAges = Refl

