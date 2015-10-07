module Main where

import Control.Applicative
import Control.Monad (forM)
import Data.Char (isUpper)

------------------------------------------------------------------------------

data Acronym = ADef Int String | AUse Int String | AOther Int String

mkAcronym :: Int -> String -> Acronym
mkAcronym n cs | isInParens cs = ADef n cs
               | isUppercase cs = AUse n cs
               | otherwise = AOther n cs

isADef :: Acronym -> Bool
isADef (ADef _ _) = True
isADef _          = False

isAUse :: Acronym -> Bool
isAUse (AUse _ _) = True
isAUse _          = False

isAOther :: Acronym -> Bool
isAOther (AOther _ _) = True
isAOther _            = False

getString :: Acronym -> String
getString (ADef _ cs) = cs
getString (AUse _ cs) = cs
getString (AOther _ cs) = cs

instance Show Acronym where
  show (ADef n cs) = show n ++ ", " ++ cs
  show (AUse n cs) = show n ++ ", " ++ cs
  show (AOther n cs) = show n ++ ", " ++ cs

lift :: (String -> Bool) -> Acronym -> Bool
lift f = f . getString

parseFile :: String -> IO [Acronym]
parseFile fname = do
  ls <- zip [1..] <$> lines <$> (readFile fname)
  return $ concatMap (\(lN,lC) -> map (mkAcronym lN) (words lC)) ls

------------------------------------------------------------------------------

isUppercase :: String -> Bool
isUppercase = all (\c -> isUpper c || c == '(' || c == ')')

isLongerThan :: Int -> String -> Bool
isLongerThan 0 (_:_)  = True
isLongerThan 0 []     = False
isLongerThan n (_:cs) = isLongerThan (n-1) cs
isLongerThan n []     = False

isInParens :: String -> Bool
isInParens ('(':cs) = go cs
  where go [] = False
        go (")") = True
        go (c:cs) | isUpper c = go cs
                  | otherwise = False
isInParens _ = False

------------------------------------------------------------------------------

myCriteria :: Acronym -> Bool
myCriteria cs =
  all id $ [ lift isUppercase
           , lift $ isLongerThan 1
           , lift $ not . (flip elem ["TODO","SQL","LINQ","PINQ","DPDT"])
           , not . isAOther
           ] <*> pure cs

main :: IO ()
main = do
  as <- filter myCriteria <$> parseFile "main.tex"
  mapM_ print as
