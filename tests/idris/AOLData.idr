module AOLData

import Database.DPDT.Idris

Query : Schema -> Stability -> Type
Query = Query ListRow

AOLSchema : Schema
AOLSchema = [ "AnonID" ::: String, "Query" ::: String ]

parseLine : String -> Row AOLSchema
parseLine cs = let (x::y::zs) = words cs
                in [ x , y ]

loadTable : String -> IO (Query AOLSchema 1)
loadTable fname = do
  f <- map parseLine . lines <$> readFile fname
  return $ MkQuery (Table f)

main : IO ()
main = do
  t <- loadTable "tests/idris/AOL_sm.txt"
  return ()

