-- lucas silva lopes do carmo
-- 202110

import Control.Monad (forM_)
import Data.Char
import Data.List
import qualified Data.Map as Map

main = do
  line <- getLine
  if null line
    then return ()
    else do
      let frequencyTupleList = [('1', 0), ('2', 0), ('3', 0), ('4', 0), ('5', 0), ('6', 0), ('7', 0), ('8', 0), ('9', 0)]
      let totalNumber = length $ filter (/= ' ') $ sort $ lineFirstDigits line
      let frequency = map (\x -> (head x, length x)) $ group $ filter (/= ' ') $ sort $ lineFirstDigits line
      let results = map (\(a, b) -> (a, fromIntegral b / fromIntegral totalNumber)) $ Map.toList $ Map.fromListWith (+) $ frequencyTupleList ++ frequency
      forM_ results (putStrLn . formatTuple)

lineFirstDigits :: String -> String
lineFirstDigits [] = []
lineFirstDigits line = unwords $ map wordFirstDigit $ words line

wordFirstDigit :: String -> [Char]
wordFirstDigit [] = []
wordFirstDigit (x : xs) =
  if isDigit x && x > '0'
    then [x]
    else wordFirstDigit xs

formatTuple :: Show b => (Char, b) -> String
formatTuple (a, b) = [a] ++ " : " ++ show b
