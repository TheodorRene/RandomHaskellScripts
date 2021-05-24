module Main where

import Data.List (transpose)

main :: IO ()
main = print "hello"

userrating =
  [ [1, 0, 3, 0, 0, 5, 0, 0],
    [0, 0, 5, 4, 0, 0, 4, 0],
    [2, 4, 0, 1, 2, 0, 3, 0],
    [0, 2, 4, 0, 5, 0, 0, 4],
    [0, 0, 4, 3, 4, 2, 0, 0],
    [1, 0, 3, 0, 3, 0, 0, 2]
  ] ::
    [[Double]]

userratingSubtractMean :: [[Double]]
userratingSubtractMean = zipWith fun mean userrating
    where
    mean = map avg userrating
    fun m = map (\num -> if num > 0 then num - m else 0)

columns = transpose userrating

-- fetch vectors from file
iouserrrating = map split' . lines <$> readFile "vectors.txt"

six = userrating !! 5
sixMean = userratingSubtractMean !! 5

pearsons1toMany l fullSet = map (pearson l) $ filter (l/=) fullSet
cosine1ToMany l fullSet = map (cosineSim l) $ filter (l/=) fullSet

pp :: [[Double]] -> IO ()
pp = mapM_ print

split' :: String -> [Double]
split' str = split str []

split :: String -> [Double] -> [Double]
split (x : xs) l
  | x == ',' = split xs l
  | otherwise = split xs (l ++ [read [x] :: Double])
split [] l = l

-- [Double] -> [Bits] basically
userratingBits = map valueToOne userrating

valueToOne :: [Double] -> [Double]
valueToOne = map (\x -> if x > 0 then 1 else 0)

-- Σ(A-meanA)*(B-meanB)
meanSumVectorMultiplication avgA avgB a b = sum $ zipWith (*) normalizedA normalizedB
  where
    subtractMean average vector = map (subtract average) vector
    normalizedA = subtractMean avgA a
    normalizedB = subtractMean avgB b

-- ΣA*B
vectorSumMultiplication = meanSumVectorMultiplication 0 0

cosineSim a b = vectorSumMultiplication a b / (sumLengthVector a * sumLengthVector b)

-- √ΣA²
sumLengthVector = sqrt . sum . map (^ 2)

-- √Σ(A-meanA)²
meanSumLengthVector average = sumLengthVector . map (subtract average)

jaccard a b = nrSameItems / nrAllItems
  where
    nrSameItems = sum $ zipWith (\x y -> boolToNum (x == y && x /= 0)) a b
    nrAllItems = sum $ zipWith (\x y -> boolToNum (x == 1 || y == 1)) a b

boolToNum True = 1
boolToNum False = 0

-- S_xy
removeUnratedRecords a b = unzip . filter (\(x, y) -> x /= 0.0 && y /= 0.0) $ zip a b

-- Bit unsure if this is correct
pearson a b = weightedAverage / (meanSumLengthVector avg_a l1 * meanSumLengthVector avg_b l2)
  where
    weightedAverage = meanSumVectorMultiplication avg_a avg_b l1 l2
    normalize aveg = map (subtract aveg)
    avg_a = avg a
    avg_b = avg b
    (l1, l2) = removeUnratedRecords a b

avg l = sum l / fromIntegral (length $ filter (/=0) l)

weightedAverage1 (vectors, sim) itemIndex = numerator / denominator
  where
    numerator = sum $ map (* (sim !! itemIndex)) (vectors !! itemIndex)
    denominator = sum sim
