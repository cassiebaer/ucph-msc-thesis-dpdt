module Database.PowerOfPi.Backends

import Database.PowerOfPi.Abstract.Types
import Database.PowerOfPi.Idris.Types

data Backend = Idris | SQLite

%assert_total
TableType : Backend -> Schema -> Type
TableType Idris  s = List (Row s)
TableType SQLite _ = String
