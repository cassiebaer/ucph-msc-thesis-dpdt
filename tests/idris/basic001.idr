module Basic001

import Database.PowerOfPi.Idris

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

unionPeopleWithItself : ListRow Person
unionPeopleWithItself = eval (people `Union` people)

lengthUnionPeopleWithItself : length Basic001.unionPeopleWithItself = 8
lengthUnionPeopleWithItself = Refl

unionPeopleWithNew : ListRow Person
unionPeopleWithNew = eval (people `Union` (Table [["Alice",18]]))

lengthUnionPeopleWithNew : length Basic001.unionPeopleWithNew = 5
lengthUnionPeopleWithNew = Refl

diffPeopleWithItself : ListRow Person
diffPeopleWithItself = eval (people `Diff` people)

lengthDiffPeopleWithItself : length Basic001.diffPeopleWithItself = 0
lengthDiffPeopleWithItself = Refl

diffPeopleWithNew : List (Row Person)
diffPeopleWithNew = eval (people `Diff` (Table [["Gismo",2]]))

lengthDiffPeopleWithNew : length Basic001.diffPeopleWithNew = 3
lengthDiffPeopleWithNew = Refl

prodPeopleWithABC : List (Row (Person ++ ["Foo":::Char]))
prodPeopleWithABC = eval (Product people fooTable) where
  fooTable : Query ["Foo":::Char]
  fooTable = Table [ [ 'A' ] , [ 'B' ] , [ 'C' ] ]

lengthProdPeopleWithABC : length Basic001.prodPeopleWithABC = 12
lengthProdPeopleWithABC = Refl

projPeopleFirstNames : List (Row ["FirstName":::String])
projPeopleFirstNames = eval (Projection fooProj people) where
  fooProj : String -> Maybe String
  fooProj "Name" = Just "FirstName"
  fooProj _      = Nothing

selectOneTable : List (Row Person)
selectOneTable = eval (Select expr people) where
  expr : Expr Person Bool
  expr = (Person ^ "Age") == (Lit 25)

lengthSelectOneTable : length Basic001.selectOneTable = 1
lengthSelectOneTable = Refl

groupByAge : GroupingMap Nat Person
groupByAge = eval (MkGrouping (Person ^ "Age") people)

lengthGroupByAge : length Basic001.groupByAge = 3
lengthGroupByAge = Refl

twentySixYOs : List (Row Person)
twentySixYOs = eval $ Lookup 26 $ MkGrouping (Person ^ "Age") people

length26YOs : length Basic001.twentySixYOs = 2
length26YOs = Refl

countTable : eval (count Basic001.people) = length (eval Basic001.people)
countTable = Refl

sumAges : eval (sum Basic001.people (Person^"Age")) = 79
sumAges = Refl

partitionByAge : GroupingMap Nat Person
partitionByAge = eval (MkPartitioning [2,26] (Person^"Age") people)

