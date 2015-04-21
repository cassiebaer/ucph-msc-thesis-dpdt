module Dates where

import Data.List
import Data.Time.Calendar

startDate = fromGregorian 2015 4 20
dates = [ ("Thesis Start:   ",startDate)
        , ("Thesis End:     ",addGregorianMonthsRollOver 6 startDate)
        , ("Thesis Defense: ",addGregorianMonthsRollOver 7 startDate)
        , ("ToUSA:          ",fromGregorian 2015 5 13)
        , ("  Return:       ",fromGregorian 2015 6 23)
        , ("ToUSA (*):      ",fromGregorian 2015 8 01)
        , ("  Return (*):   ",fromGregorian 2015 9 10)
        ]

showDate :: (String,Day) -> String
showDate (cs,d) = cs ++ showGregorian d

main :: IO ()
main = do
    mapM_ (putStrLn . showDate) (sortBy (\(_,d) (_,d') -> compare d d') dates)
    putStrLn "\n(*) Approximate date"
