module Database.DPDT.SQLite

import public System.Random.CrapGen
import public Data.Rational
import public Database.PowerOfPi.SQLite
import public Database.DPDT

%default total


||| Uniformly draws a random variable in range [-1, 1)
rndVar : String
rndVar = "(random() / 9223372036854775808)"

signum : String -> String
signum x = "(CASE WHEN " ++ x ++ " < 0 THEN -1 ELSE 1 END)"

samplePure : Double -> Double -> String
samplePure mu b = "("   ++ show mu ++
                  " - " ++ show b ++
                  " * " ++ signum rndVar ++
                  " * log(1 - 2 * abs(" ++ rndVar ++ "))"

namespace Query

  Query : Schema -> Stability -> Type
  Query = Query SQLiteTable

  noisyCount : (Query s c) -> (e:Epsilon) -> Private (c*e) String
  noisyCount  (MkQuery q) eps  = MkPrivate $ \g =>
       let noise = "samplePure(0, " ++ show (1 / toFloat eps) ++ ")"
       in ("SELECT (COUNT(*) + " ++ noise ++ ") FROM (" ++ eval q ++ ")", g)

  noisyAverage : Expr s Double -> Query s c -> (e:Epsilon) -> Private (c*e) String
  noisyAverage exp (MkQuery q) eps = MkPrivate $ \g =>
       let expStr  = eval exp
           clampLT = "CASE WHEN " ++ expStr ++ " < -1 THEN -1 ELSE " ++ expStr ++ " END"
           clamp   = "CASE WHEN " ++ expStr ++ " > 1 THEN 1 ELSE ("  ++ clampLT ++ ") END"
           width   = show $ 2 / toFloat eps
           noisyAvg = "noisyAverage(AVG(ClampedValues), " ++ width ++ ")"
           isEmpty = "CASE WHEN COUNT(*) == 0 THEN " ++ rndVar ++ " ELSE " ++ noisyAvg ++ " END"
           select  = "SELECT (" ++ isEmpty ++ ") "
           from    = "FROM (SELECT (" ++ clamp ++ ") AS ClampedValues FROM (" ++ eval q ++ "))"
       in  (select ++ from , g)

namespace Grouping

  Grouping : Schema -> Type -> Stability -> Type
  Grouping = Grouping SQLiteTable

  noisyCount : (Grouping s k c) -> (e:Epsilon) -> Private (c*e) String
  noisyCount  (MkGrouping q) eps  = MkPrivate $ \g =>
       let noise = "samplePure(0, " ++ show (1 / toFloat eps) ++ ")"
       in ("SELECT (COUNT(*) + " ++ noise ++ ") FROM (" ++ eval q ++ ")", g)

