module Main where

import           Text.Read                      ( readMaybe )
import           Data.Maybe                     ( mapMaybe )

import Data.List(sort)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lin = sort $ mapMaybe intoNums $ lines contents
  let extra = maximum lin + 3
  let part1 = count1and3 ([0]++lin++[extra]) (0,0)
  print part1


-- Parse list of Strings into Maybe Int.
intoNums :: String -> Maybe Int
intoNums str_num = readMaybe str_num :: Maybe Int

-- Count 1s and 3s
count1and3::[Int] -> (Int,Int) -> Int
count1and3 [] (ones,threes) = ones * threes
count1and3 [_] (ones,threes)= ones * threes
count1and3 (x:z:xs) (ones, threes) 
    | diff  == 1 = count1and3 (z:xs) (ones+1, threes)
    | diff == 3 = count1and3 (z:xs) (ones, threes+1)
    | otherwise = count1and3 (z:xs) (ones, threes)
    where diff = z-x


