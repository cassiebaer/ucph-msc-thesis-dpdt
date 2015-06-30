module PINQuery

import Database.PowerOfPi
import Database.PINQ
import System.Random.CrapGen
import Statistics.Distribution.Laplace
import Database.Backend.SQLite.Query
import public Database.Backend.PINQuery

instance Aggregation (PINQuery SQLite) where
  noisyCount (MkPINQuery q) e = MkPrivate $ \g => 
         let (rx,g') = rndDouble g
             noise   = samplePure 0 (1 / toFloat e) (rx-0.5)
             count   = fromInteger $ fromNat $ length (eval q)
         in (count + noise, g')

