module Database.DPDT.SQLite

import public System.Random.CrapGen
import public Data.Rational
import public Database.PowerOfPi.SQLite
import public Database.DPDT

%default total


||| Uniformly draws a random variable from (-0.5, 0.5)
rndVar : String
rndVar = "(random() / 18446744073709551616)"

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

  noisyCount : (Query SQLiteTable s c) -> (e:Epsilon) -> Private (c*e) String
  noisyCount  (MkQuery q) eps  = MkPrivate $ \g =>
       let noise = "samplePure(0, " ++ show (1 / toFloat eps) ++ ")"
       in ("SELECT (COUNT(*) + " ++ noise ++ ") FROM (" ++ eval q ++ ")", g)

namespace Grouping

  Grouping : (Num k, Show k) => Schema -> Type -> Stability -> Type
  Grouping = Grouping SQLiteTable

  noisyCount : (Grouping SQLiteTable s k c) -> (e:Epsilon) -> Private (c*e) String
  noisyCount  (MkGrouping q) eps  = MkPrivate $ \g =>
       let noise = "samplePure(0, " ++ show (1 / toFloat eps) ++ ")"
       in ("SELECT (COUNT(*) + " ++ noise ++ ") FROM (" ++ eval q ++ ")", g)

