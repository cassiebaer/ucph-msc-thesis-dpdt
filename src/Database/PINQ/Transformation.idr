module Database.PINQ.Transformation

import Database.PowerOfPi.Expr
import Database.PINQ.Types

class Transformation (pinq : Schema -> Stability-> Type) where
    where'     : pinq  s c -> Expr s Bool -> pinq  s c
    select     : pinq  s c -> (f:String -> Maybe  String) -> pinq  (projectedSchema f s) c
    union      : pinq  s c -> pinq  s c' -> pinq  s (c + c')
    intersect  : pinq  s c -> pinq  s c' -> pinq  s (c + c')
    groupBy    : pinq  s c -> Expr s k -> pinq ["k":::k, "v"::: pinq s (c * 2)] (c * 2)
