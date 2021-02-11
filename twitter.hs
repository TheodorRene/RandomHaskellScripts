module Main where
    
main :: IO ()

{- https://twitter.com/Al_Grigor/status/1357028887209902088
-- Most candidates cannot solve this interview problem:

Small orange diamond
 Input: "aaaabbbcca"
Small orange diamond
 Output: [("a", 4), ("b", 3), ("c", 2), ("a", 1)]

Write a function that converts the input to the output

I ask it in the screening interview and give it 25 minutes

How would you solve it?
-}

testString = "aaaabbbcca"

main =  print $ doTask testString

getList (Counter list _) = list

doTask::String -> [(Char,Int)]
doTask (x:xs) = getList $ foldl countR (Counter [] (x,1)) (xs++" ")

data Counter = Counter [(Char,Int)] (Char,Int) deriving (Show)

countR :: Counter -> Char -> Counter
countR (Counter list tup@(char,count)) curr
    | char == curr = Counter list (char, count+1)
    | otherwise = Counter (list ++ [tup]) (curr,1)

