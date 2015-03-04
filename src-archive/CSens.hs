import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM2)
import Test.QuickCheck

class Dist t1 where
    dist :: (t1,t1) -> Double

instance Dist Double where
    dist (x,y) = abs (x - y)

newtype SumProd t1 t2 = SumProd (t1,t2) -- "(x,y)"
    deriving (Show)
newtype MaxProd t1 t2 = MaxProd (t1,t2) -- "<x,y>"
    deriving (Show)

instance (Dist t1, Dist t2) => Dist (SumProd t1 t2) where
    dist (SumProd (x,y), SumProd (x',y')) = dist (x,x') + dist (y,y')

instance (Dist t1, Dist t2) => Dist (MaxProd t1 t2) where
    dist (MaxProd (x,y), MaxProd (x',y')) = max (dist (x,x')) (dist (y,y'))

instance (Arbitrary t1, Arbitrary t2) => Arbitrary (SumProd t1 t2) where
    arbitrary = SumProd <$> ((,) <$> arbitrary <*> arbitrary)

instance (Arbitrary t1, Arbitrary t2) => Arbitrary (MaxProd t1 t2) where
    arbitrary = MaxProd <$> ((,) <$> arbitrary <*> arbitrary)

------------------------------------

fnIsCSensitive :: (Arbitrary t1, Dist t1, Dist t2) => (t1 -> t2) -> Double -> (t1,t1) -> Bool
fnIsCSensitive f c (x,y) = dist (f x,f y) <= c * dist (x,y)

oneSens = 1.000001 -- to avoid floating point errors

prop_exOf1SensFns :: IO ()
prop_exOf1SensFns = do
    pHeader "f1-5"
    quickCheck $ fnIsCSensitive (\x -> x              :: Double) oneSens
    quickCheck $ fnIsCSensitive (\x -> (-x)           :: Double) oneSens
    quickCheck $ fnIsCSensitive (\x -> x/2            :: Double) oneSens
    quickCheck $ fnIsCSensitive (\x -> abs x          :: Double) oneSens
    quickCheck $ fnIsCSensitive (\x -> (x + abs x)/2  :: Double) oneSens
    pHeader "f8-12"
    quickCheck $ fnIsCSensitive (\(SumProd (x,y)) -> x + y      :: Double) oneSens
    quickCheck $ fnIsCSensitive (\(SumProd (x,y)) -> x - y      :: Double) oneSens
    quickCheck $ fnIsCSensitive (\(SumProd (x,y)) -> SumProd (x,y)      :: SumProd Double Double) oneSens
    quickCheck $ fnIsCSensitive (\(SumProd (x,y)) -> SumProd (y,x)      :: SumProd Double Double) oneSens
    quickCheck $ fnIsCSensitive (\(SumProd (x,y)) -> SumProd (x+y,0)    :: SumProd Double Double) oneSens
    pHeader "cswap"
    quickCheck $ fnIsCSensitive (\(SumProd (x,y)) -> if x < y
                                                     then SumProd (x,y)
                                                     else SumProd (y,x) :: SumProd Double Double) oneSens
    pHeader "f15"
    quickCheck $ fnIsCSensitive (\(SumProd (x,y)) -> let types = (x,y) :: (Double,Double)
                                                      in MaxProd (x,x) :: MaxProd Double Double) oneSens

pHeader :: String -> IO ()
pHeader cs = do
    putStr "\n\n"
    putStrLn "----------------------------------------------------------------"
    putStrLn $ "--      " ++ cs
    putStrLn "----------------------------------------------------------------"

main :: IO ()
main = do
    prop_exOf1SensFns
