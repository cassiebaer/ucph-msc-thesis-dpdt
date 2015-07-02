module Database.Backend.Idris.PINQuery

import Database.PowerOfPi
import Database.PINQ
import Database.Backend.Idris.Expr
import System.Random.CrapGen
import Statistics.Distribution.Laplace
import Database.Backend.Idris.Query
import public Database.Backend.PINQuery
%default total

%assert_total
until : (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where
    go x = if p x then x else go (f x)

instance Aggregation (PINQuery Idris) where
  noisyCount (MkPINQuery q) e = MkPrivate $ \g => 
    let (rx,g') = rndDouble g
        noise   = samplePure 0 (1 / toFloat e) (rx-0.5)
        count   = fromInteger $ fromNat $ length (eval q)
    in (count + noise, g')
  noisyAverage exp (MkPINQuery q) eps = MkPrivate $ \g =>
    let rs      = map (clamp . eval exp) (eval q)
        (rx,g') = rndDouble g
        noise   = samplePure 0 (1 / toFloat eps) (rx-0.5)
        tally   = sum rs
        count   = length rs
     in if count == 0 then ((rx - 0.5)*2,g')
                      else until (\(rx,g) => (-1.0) < rx + tally
                                          && rx + tally < 1.0)
                                 (\(rx,g) => rndDouble g)
                                 (100.0,g')
    where clamp : Double -> Double
          clamp x = if x > 1.0 then 1.0 else
                    if x < (-1.0) then (-1.0) else x

