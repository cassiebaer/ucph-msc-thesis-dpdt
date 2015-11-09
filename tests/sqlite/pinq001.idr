module PINQ001

import Database.DPDT.SQLite
import System.Random.CrapGen

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Nat ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : Query Person 1
people = MkQuery $ Table "People"

foods : Query Food 1
foods = MkQuery ( Table "Foods")


--- TODO: count as a SQLite query
namespace Aggregations

  testNoisyCount : String
  testNoisyCount = evalPrivate $ noisyCount people 1
                                   

  --avgAge :
  --avgAge = noisyMean (MkPINQuery (Select (PureFn (>20) (Person^"Age"))
                                         --people))


