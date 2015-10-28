module Basic001

import Database.PowerOfPi.Idris

Query : Schema -> Type
Query = Query ListRow

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Nat ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : Query Person
people = Table [ [ "Casper" , 25 ]
               , [ "Knut"   , 26 ]
               , [ "Tor"    , 26 ]
               , [ "Gismo"  ,  2 ]
               ]

foods : Query Food
foods = Table [ [ "Casper" , "Bruschetta" ]
              , [ "Knut"   , "Prim"       ]
              , [ "Gismo"  , "Dog food"   ]
              ]

namespace Union

  unionPeopleWithItself : ListRow Person
  unionPeopleWithItself = eval (people `Union` people)

  lengthUnionPeopleWithItself : length unionPeopleWithItself = 8
  lengthUnionPeopleWithItself = Refl

  unionPeopleWithNew : ListRow Person
  unionPeopleWithNew = eval (people `Union` (Table [["Alice",18]]))

  lengthUnionPeopleWithNew : length unionPeopleWithNew = 5
  lengthUnionPeopleWithNew = Refl

namespace Diff

  diffPeopleWithItself : ListRow Person
  diffPeopleWithItself = eval (people `Diff` people)

  lengthDiffPeopleWithItself : length diffPeopleWithItself = 0
  lengthDiffPeopleWithItself = Refl

  diffPeopleWithNew : List (Row Person)
  diffPeopleWithNew = eval (people `Diff` (Table [["Gismo",2]]))

  lengthDiffPeopleWithNew : length diffPeopleWithNew = 3
  lengthDiffPeopleWithNew = Refl

namespace Product

  prodPeopleWithABC : List (Row (Person ++ ["Foo":::Char]))
  prodPeopleWithABC = eval (Product people fooTable) where
    fooTable : Query ["Foo":::Char]
    fooTable = Table [ [ 'A' ] , [ 'B' ] , [ 'C' ] ]

  lengthProdPeopleWithABC : length prodPeopleWithABC = 12
  lengthProdPeopleWithABC = Refl

namespace Projection

  projPeopleFirstNames : List (Row ["FirstName":::String])
  projPeopleFirstNames = eval (Projection fooProj people) where
    fooProj : String -> Maybe String
    fooProj "Name" = Just "FirstName"
    fooProj _      = Nothing

namespace Select

  selectOneTable : List (Row Person)
  selectOneTable = eval (Select expr people) where
    expr : Expr Person Bool
    expr = (Person ^ "Age") == (Lit 25)

  lengthSelectOneTable : length selectOneTable = 1
  lengthSelectOneTable = Refl

namespace Grouping

  groupByAge : GroupingMap Nat Person
  groupByAge = eval (MkGrouping (Person ^ "Age") people)

  lengthGroupByAge : length groupByAge = 3
  lengthGroupByAge = Refl

namespace Foo

  twentySixYOs : List (Row Person)
  twentySixYOs = eval $ Lookup 26 $ MkGrouping (Person ^ "Age") people

  length26YOs : length twentySixYOs = 2
  length26YOs = Refl

namespace Aggregation

  countTable : eval (count people) = length (eval people)
  countTable = Refl

  sumAges : eval (sum people (Person^"Age")) = 79
  sumAges = Refl

namespace Partition

  partitionByAge : GroupingMap Nat Person
  partitionByAge = eval (MkPartitioning [2,26] (Person^"Age") people)

