module PINQ001
import Database.PowerOfPi
import Database.PINQ.Aggregations
import Database.PINQ.PINQuery
import Database.PINQ.Types
import System.Random.CrapGen

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Nat ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : PINQuery Person 1
people = MkLeaf $ Table [ [ "Casper" , 25 ]
                            , [ "Knut"   , 26 ]
                            , [ "Gismo"  ,  2 ]
                            ]

foods : PINQuery Food 1
foods = MkLeaf ( Table [ [ "Casper" , "Bruschetta" ]
                           , [ "Knut"   , "Prim"       ]
                           , [ "Gismo"  , "Dog food"   ]
                           ])

namespace Aggregations

  testNoisyCount : Double
  testNoisyCount = evalPrivate (do x <- noisyCount people 1
                                   y <- noisyCount people 1
                                   return ((x+y)/2)
                               ) 123

  --avgAge : 
  --avgAge = noisyMean (MkPINQuery (Select (PureFn (>20) (Person^"Age"))
                                         --people))


