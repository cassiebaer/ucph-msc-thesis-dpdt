module Database.DPDT.PINQueryable

import Database.DPDT.Types

class PINQueryable (pinq : Type -> Type) where

-- Aggregations

    noisyCount   : pinq a -> Eps -> Double
    noisyAverage : pinq a -> Eps -> (a -> Double) -> Double
    noisySum     : pinq a -> Eps -> (a -> Double) -> Double
    noisyMedian  : pinq a -> Eps -> (a -> Double) -> Double
    exponentialMechanism : pinq a -> Eps -> IQueryable b -> (a -> b -> Double) -> b

-- Transformations

    where'     : pinq a -> (a -> Bool) -> pinq a
    select     : pinq a -> (a -> b) -> pinq b
    selectMany : pinq a -> Nat -> (a -> IEnumerable b) -> pinq b

    take : pinq a -> Nat -> pinq a
    skip : pinq a -> Nat -> pinq a

    join      : pinq a -> pinq b -> (a -> k) -> (b -> k)
             -> (IGrouping k a -> IGrouping k b -> r) -> pinq r
    groupJoin : pinq a -> pinq b -> (a -> k) -> (b -> k)
             -> (IGrouping k a -> IEnumerable b -> r) -> pinq r
    concat    : pinq a -> pinq a -> pinq a
    groupBy   : pinq a -> (a -> k) -> pinq (IGrouping k a)

    distinct     : pinq a -> pinq a
    union        : pinq a -> pinq a -> pinq a
    intersection : pinq a -> pinq a -> pinq a
    except       : pinq a -> pinq a -> pinq a
