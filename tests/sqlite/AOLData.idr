module Main

import Database.DPDT.SQLite
import Effects
import Effect.StdIO

AOLSchema : Schema
AOLSchema = [ "AnonID" ::: Double, "Query" ::: String ]

table : Query AOLSchema 1
table = MkQuery (Table "AOL")

countUniqueAnonId : Private 2 String
countUniqueAnonId = 
  noisyCount gq 1 where
  gq : Grouping AOLSchema Double 2
  gq = groupBy (AOLSchema^"AnonID") table
  
averageAnonId : Private 1 String
averageAnonId = noisyAverage (AOLSchema^"AnonID") table 1

printResult : Eff () [STDIO]
printResult = do
              putStr $ evalPrivate countUniqueAnonId 33 ++ "\n"
              putStr $ evalPrivate averageAnonId 612212 ++ "\n"

main : IO ()
main = run printResult
