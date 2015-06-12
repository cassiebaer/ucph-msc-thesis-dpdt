module PINQuery

import Database.PowerOfPi
import Database.PINQ
import Database.Backend.Backend

data PINQuery : Backend -> Schema -> Stability -> Type  where
  MkPINQuery : Query b s -> PINQuery b s c
  Grouping   : Query b ["k":::k, "v":::Query b s] -> PINQuery b ["k":::k, "v":::PINQuery b s c] c


instance Transformation (PINQuery b) where
  where'    (MkPINQuery  q) e                = MkPINQuery  (Select e q)
  select    (MkPINQuery  q) f                = MkPINQuery  (Projection f q)
  union     (MkPINQuery  q) (MkPINQuery  q') = MkPINQuery  (Union q q')
  intersect (MkPINQuery  q) (MkPINQuery  q') = MkPINQuery  (Diff q q')
  groupBy   (MkPINQuery  q) e                = Grouping    (GroupBy e q)
