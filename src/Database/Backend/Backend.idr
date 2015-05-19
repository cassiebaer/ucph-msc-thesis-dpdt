module Database.Backend.Backend

import Database.PowerOfPi.Types
import Database.Backend.Idris.Row

data Backend = Idris

TableType : Backend -> Schema -> Type
TableType Idris s = List (Row s)
