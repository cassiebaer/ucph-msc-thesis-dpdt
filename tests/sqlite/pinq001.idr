module PINQ001

import Database.PINQ.SQLite
import System.Random.CrapGen

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Nat ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : PINQuery SQLite Person 1
people = MkPINQuery $ Table "People"

foods : PINQuery SQLite Food 1
foods = MkPINQuery ( Table "Foods")


--- TODO: count as a SQLite query
namespace Aggregations

  testNoisyCount : Double
  testNoisyCount = evalPrivate (do x <- noisyCount people 1
                                   y <- noisyCount people 1
                                   return ((x+y)/2)
                               ) 123

  --avgAge : 
  --avgAge = noisyMean (MkPINQuery (Select (PureFn (>20) (Person^"Age"))
                                         --people))


