module Main where
    import Data.List.Split
    import Data.List (union)
    import Data.Set (Set, size, fromList, empty, intersection)

    main :: IO ()
    main = do
        contents <- readFile "input.txt"
    --   let lines_input = lines contents
    --    let parsed_data = convertToBetterRepresentation lines_input
        let parsed_data2 = convertToBetterRepresentation2 contents
        let part2_sum = calculateData parsed_data2
        -- let reddit = sum $ map (length . foldr1 union) parsed_data
        print part2_sum

    type Group = [String]

    convertToBetterRepresentation2 :: String -> [Group]
    convertToBetterRepresentation2 input =
        map lines $ splitOn "\n\n" input


    reduceToSet :: String -> Set Char -> Set Char
    reduceToSet curr acc =
        intersection acc (fromList curr)

    reduceFunc :: Group -> Int
    reduceFunc string =
        size $ foldr reduceToSet (fromList $ head string) string
    
    calculateData :: [Group] -> Int
    calculateData parsed_data =
        sum $ map reduceFunc parsed_data

    emptyStringToSpace :: String -> String
    emptyStringToSpace x
      | x == "" = " "
      | otherwise = x++","

    mapGroupss group =
        size $ fromList group 

    convertToBetterRepresentation :: [String] -> [Group]
    convertToBetterRepresentation input =
        listOfLists $ map init $ splitOn " " $ concatMap emptyStringToSpace input


    listOfLists::[String] -> [Group]
    listOfLists =
        map (splitOn ",")

