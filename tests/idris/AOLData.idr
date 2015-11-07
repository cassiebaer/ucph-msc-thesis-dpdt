module Main

import Database.DPDT.Idris

AOLSchema : Schema
AOLSchema = [ "AnonID" ::: String, "Query" ::: String ]

parseLine : String -> Row AOLSchema
parseLine cs = let (x::y::zs) = words cs
                in [ x , y ]

loadTable : String -> IO (Query AOLSchema 1)
loadTable fname = do
  f <- map parseLine . lines <$> readFile fname
  return $ MkQuery (Table f)

-- TODO 1 : FIX THIS! We need associativity: esp. with (fromInteger 1). Commutativity would be nice, too.
-- (Try changing c*2*1 to anything else.)

countUniqueAnonID : Query AOLSchema c -> Private (c*2*1) Double
countUniqueAnonID q = do
  let gq = groupBy (AOLSchema^"AnonID") q
  noisyCount gq 1

main : IO ()
main = do
  t <- loadTable "tests/AOL_sm.txt"
  print $ evalPrivate (countUniqueAnonID t) 1238389612
  return ()
