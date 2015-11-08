module Tests.AOLData

import Data.Rational
import Database.PowerOfPi
import Database.DPDT

%default total

AOLSchema : Schema
AOLSchema = [ "AnonID" ::: String, "Query" ::: String ]

GroupByAnonID : Query b AOLSchema c -> Database.DPDT.Grouping b AOLSchema String (c*2)
GroupByAnonID q = groupBy (AOLSchema^"AnonID") q

