module PINQuery

import Data.Rational
import Database.PowerOfPi
import Database.PINQ.Types
import Database.PINQ.Aggregations
import Statistics.Distribution.Laplace
import System.Random.CrapGen
import Database.PINQ.PINQueryable
import Data.Double

data PINQuery : Schema -> Stability -> Type where
  MkPINQuery : Query Idris s -> PINQuery s c 
 
instance PINQueryable PINQuery where
  where'     (MkPINQuery q) e = MkPINQuery (Select e q)
  select     (MkPINQuery q) f = MkPINQuery (Projection f q)
  union      (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Union q q')
  intersect  (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Diff q q')
  noisyCount (MkPINQuery q) e = MkPrivate $ \g => 
    let (rx,g') = rndDouble g
        noise   = samplePure 0 (1 / toFloat e) (rx-0.5)
        count   = fromInteger $ fromNat $ length (eval q)
    in (count + noise, g')
