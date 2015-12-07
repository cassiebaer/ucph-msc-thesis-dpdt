module Main

import Data.Vect
import Database.DPDT.Idris
import Statistics.Distribution.Laplace
import Statistics.Distribution.Summary
import System.Random.CrapGen

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Double ]

Food : Schema
Food = [ "Name" ::: String , "Food" ::: String ]

people : Query Person 1
people = MkQuery $ Table [ [ "Alice"  , 40 ]
                         , [ "Casper" , 26 ]
                         , [ "Knut"   , 26 ]
                         , [ "Tor"    , 26 ]
                         , [ "Gismo"  ,  2 ]
                         ]

foods : Query Food 1
foods = MkQuery ( Table [ [ "Casper" , "Bruschetta" ]
                        , [ "Knut"   , "Prim"       ]
                        , [ "Gismo"  , "Dog food"   ]
                        ])

namespace Aggregations

  countAlices : Private 1 Double
  countAlices = do let isAlice = Person^"Name" == Lit "Alice"
                   let alices  = people `where'` isAlice
                   noisyCount alices 1

  countAlicesN : (n:Nat) -> Vect n Double
  countAlicesN n = map (evalPrivate countAlices) (map snd $ unfoldCrapGenN n 1234567890)

  testCountAlices : Double
  testCountAlices = evalPrivate countAlices 128912839283

  tripleCountAlice : Private 3 Double
  tripleCountAlice = do let alices = people `where'` (Person^"Name" == Lit "Alice")
                        x <- noisyCount alices 1
                        y <- noisyCount alices 1
                        z <- noisyCount alices 1
                        return ((x+y+z)/3)

  testSequence : Private 1 (Vect 3 Double)
  testSequence = let alices = people `where'` (Person^"Name" == Lit "Alice")
                     p = noisyCount alices (1//3)
                  in sequence [p,p,p]

  testNoisyCount : Double
  testNoisyCount = evalPrivate (do x <- noisyCount people 1
                                   y <- noisyCount people 1
                                   return ((x+y)/2)
                               ) 123

  testNoisyAverage : Double
  testNoisyAverage = evalPrivate (do avg <- noisyAverage (PureFn (/26.0) (Person^"Age")) people 1
                                     return avg) 123

  testProofSearch : Query Person c -> Private ?proofSearch Double
  testProofSearch q = do
    x <- noisyCount q (1//10)
    y <- noisyCount q (2//10)
    return x

  testRatReduction : { e : Epsilon } -> { q : Query Person c } -> { p : Private s Double }
                  -> (noisyCount q e >>= Idris.return) = p -> c * e = s
  testRatReduction Refl = Refl

  testWithOpenStabilities : Eq k => Grouping Person k c -> k -> Private ?d Double
  testWithOpenStabilities gr k = do
    let q = lookup k gr
    x <- noisyCount q (1//10)
    y <- noisyCount q (2//10)
    return (x+y)

testLaplaceN : Nat -> IO ()
--testNLaplace n = putSummary (map (samplePure 0 0.1) (map fst $ unfoldCrapGenN n 1234567890))
testLaplaceN _ = putSummary (map (samplePure 0 1) trueRandoms)


