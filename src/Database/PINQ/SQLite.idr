module Database.PINQ.SQLite

import public Data.Rational
import public Database.PowerOfPi.SQLite -- TODO : should be public?
import public Database.PINQ

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


noisyCount : (PINQuery SQLiteTable s c) -> (e:Epsilon) -> Private (c*e) String
noisyCount  (MkPINQuery q) eps  = MkPrivate $ \g =>
    let noise = samplePure 0 (1 / toFloat eps)
     in ("SELECT (count(*) + " ++ noise ++ ") FROM table", g)
