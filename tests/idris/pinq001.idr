module PINQ001

import Database.PowerOfPi
import Database.PINQ
import Database.Backend.PINQuery
import Database.Backend.Idris.PINQuery
import Database.Backend.Idris.Row
import System.Random.CrapGen

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Double ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : PINQuery Idris Person 1
people = MkPINQuery $ Table [ [ "Casper" , 25 ]
                            , [ "Knut"   , 26 ]
                            , [ "Gismo"  ,  2 ]
                            ]

foods : PINQuery Idris Food 1
foods = MkPINQuery ( Table [ [ "Casper" , "Bruschetta" ]
                           , [ "Knut"   , "Prim"       ]
                           , [ "Gismo"  , "Dog food"   ]
                           ])

namespace Aggregations

  testNoisyCount : Double
  testNoisyCount = evalPrivate (do x <- noisyCount people 1
                                   y <- noisyCount people 1
                                   return ((x+y)/2)
                               ) 123

  testNoisyAverage : Double
  testNoisyAverage = evalPrivate (do avg <- noisyAverage (Person^"Age") people 1
                                     return avg) 123
  --avgAge : 
  --avgAge = noisyMean (MkPINQuery (Select (PureFn (>20) (Person^"Age"))
                                         --people))


