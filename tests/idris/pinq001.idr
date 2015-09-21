module Main

import Database.PINQ.Idris
import Statistics.Distribution.Laplace
import Statistics.Distribution.Summary
import System.Random.CrapGen

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Double ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : PINQuery Table Person 1
people = MkPINQuery $ Table [ [ "Alice"  , 40 ]
                            , [ "Casper" , 26 ]
                            , [ "Knut"   , 26 ]
                            , [ "Tor"    , 26 ]
                            , [ "Gismo"  ,  2 ]
                            ]

foods : PINQuery Table Food 1
foods = MkPINQuery ( Table [ [ "Casper" , "Bruschetta" ]
                           , [ "Knut"   , "Prim"       ]
                           , [ "Gismo"  , "Dog food"   ]
                           ])

namespace Aggregations

  countAlices : Private 1 Double
  countAlices = do let isAlice = Person^"Name" == Lit "Alice"
                   let alices  = people `where'` isAlice
                   noisyCount alices 1

  countAlicesN : Nat -> List Double
  countAlicesN n = map (evalPrivate countAlices) (map snd $ unfoldCrapGenN n 1234567890)

  testCountAlices : Double
  testCountAlices = evalPrivate countAlices 128912839283

  nestedAggrs : Private 3 Double
  nestedAggrs = do x <- noisyCount people 1
                   y <- noisyCount people 2
                   return ((x+y*2)/3)

  testNoisyCount : Double
  testNoisyCount = evalPrivate (do x <- noisyCount people 1
                                   y <- noisyCount people 1
                                   return ((x+y)/2)
                               ) 123

  testNoisyAverage : Double
  testNoisyAverage = evalPrivate (do avg <- noisyAverage (PureFn (/26.0) (Person^"Age")) people 1
                                     return avg) 123

testLaplaceN : Nat -> IO ()
--testNLaplace n = putSummary (map (samplePure 0 0.1) (map fst $ unfoldCrapGenN n 1234567890))
testLaplaceN _ = putSummary (map (samplePure 0 1) trueRandoms)

main : IO ()
main = testLaplaceN 10000

