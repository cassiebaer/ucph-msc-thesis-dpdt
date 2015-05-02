module Basic001

import Database.PowerOfPi

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Nat ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : Query Person
people = Table [ [ "Casper" , 25 ]
               , [ "Knut"   , 26 ]
               , [ "Gismo"  ,  2 ]
               ]

foods : Query Food
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
    where fooTable : Query ["Foo":::Char]
          fooTable = Table [ [ 'A' ] , [ 'B' ] , [ 'C' ] ]

  lengthProdPeopleWithABC : length prodPeopleWithABC = 9
  lengthProdPeopleWithABC = Refl

namespace Projection

  projPeopleFirstNames : List (Row ["FirstName":::String])
  projPeopleFirstNames = eval (Projection fooProj people)
    where fooProj : String -> Maybe String
          fooProj "Name" = Just "FirstName"
          fooProj _      = Nothing

