module Main where
    main :: IO ()
    main = print "hello"

    
    -- Σ(A-meanA)*(B-meanB)
    meanSumVectorMultiplication avgA avgB a b = sum $ zipWith (*) normalizedA normalizedB
        where subtractMean average vector = map (subtract average) vector
              normalizedA = subtractMean avgA a
              normalizedB = subtractMean avgB b

    -- ΣA*B
    vectorSumMultiplication = meanSumVectorMultiplication 0 0 
    
    cosineSim a b = vectorSumMultiplication a b / (sumLengthVector a * sumLengthVector b)


    -- √ΣA²
    sumLengthVector = sqrt . sum . map (^2)

    -- √Σ(A-meanA)²
    meanSumLengthVector average = sumLengthVector . map (subtract average)

    jaccard a b = nrSameItems / nrAllItems
        where nrSameItems = sum  $ zipWith (\x y -> boolToNum (x == y && x /= 0)) a b
              nrAllItems = sum $ zipWith (\x y -> boolToNum (x == 0 && y == 0)) a b

    boolToNum True = 1
    boolToNum False = 0


    -- S_xy
    removeUnratedRecords a b = unzip . filter (uncurry (==)) $ zip a b

    pearson a b = weightedAverage / (meanSumLengthVector avg_a l1 * meanSumLengthVector avg_b l2)
        where weightedAverage = meanSumVectorMultiplication avg_a avg_b l1 l2
              normalize aveg = map (subtract aveg)
              avg_a = avg a
              avg_b = avg b
              (l1,l2) = removeUnratedRecords a b

    avg l = sum l / fromIntegral (length l)

