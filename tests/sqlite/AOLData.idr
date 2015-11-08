module Main

import Database.DPDT.SQLite
import Effects
import Effect.StdIO

AOLSchema : Schema
AOLSchema = [ "AnonID" ::: String, "Query" ::: String ]

table : Query AOLSchema 1
table = MkQuery (Table "AOL")

countUniqueAnonId : Private 2 String
countUniqueAnonId = 
  noisyCount gq 1 where
  gq : Grouping AOLSchema String 2
  gq = groupBy (AOLSchema^"AnonID") table
  

printResult : Eff () [STDIO]
printResult = putStr $ evalPrivate countUniqueAnonId 33 ++ "\n"

main : IO ()
main = run printResult
