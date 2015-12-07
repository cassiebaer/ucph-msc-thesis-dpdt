module Main

import Database.DPDT.Idris

AOLSchema : Schema
AOLSchema = [ "AnonID" ::: String, "Query" ::: String ]

parseLine : String -> Row AOLSchema
parseLine cs = let (x::y::zs) = split (== ',') cs
                in [ x , y ]

loadTable : String -> IO (Query AOLSchema 1)
loadTable fname = do
  Right raw <- readFile fname
  let parsed = map parseLine . lines $ raw
  return $ MkQuery (Table parsed)

countUniqueAnonID : Query AOLSchema c -> Private (c*2*1) Double
countUniqueAnonID q = do
  let gq = groupBy (AOLSchema^"AnonID") q  
  noisyCount gq 1

main : IO ()
main = do
  t <- loadTable "tests/AOL_sm.txt"
  print $ evalPrivate (countUniqueAnonID t) 1238389612
  return ()

