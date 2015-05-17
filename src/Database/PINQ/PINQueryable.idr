module Database.PINQ.PINQueryable

import Database.PowerOfPi.Expr
import Database.PINQ.Types
import Database.PINQ.Aggregations
import Data.Double

class PINQueryable (pinq : Schema -> Stability-> Type) where
    where'     : pinq s c -> Expr s Bool -> pinq s c
    select     : pinq s c -> (f:String -> Maybe String) -> pinq (projectedSchema f s) c
    union      : pinq s c -> pinq s c' -> pinq s (c + c')
    intersect  : pinq s c -> pinq s c' -> pinq s (c + c')
    noisyCount : pinq s c -> (e:Epsilon) -> Private (c*e) Double
