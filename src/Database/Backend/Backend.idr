module Database.Backend.Backend

import Database.PowerOfPi.Types
import Database.Backend.Idris.Row

data Backend = Idris | SQLite

TableType : Backend -> Schema -> Type
TableType Idris  s = List (Row s)
TableType SQLite _ = String
