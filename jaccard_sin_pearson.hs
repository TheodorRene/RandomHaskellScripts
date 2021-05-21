module Main where
    main :: IO ()
    main = print "hello"


    
    cosineSim a b = atimesb / (lengthVector a * lengthVector b)
        where atimesb = sum $ zipWith (*) a b

    lengthVector = sqrt . sum . map (^2)

    jaccard a b = nrSameItems / nrAllItems
        where nrSameItems = sum  $ zipWith (\x y -> boolToInt (x == y && x /= 0)) a b
              nrAllItems = sum $ zipWith (\x y -> boolToInt (x == 0 && y == 0)) a b
              boolToInt bool = if bool then 1 else 0

    pearson1 a b = unzip . filter (uncurry (==)) $ zip a b

    pearson a b = weightedAverage / (weightedLength avg_a l1 * weightedLength avg_b l2)
        where weightedAverage = sum $ zipWith (*) (normalize avg_a l1) (normalize avg_b l2)
              normalize aveg = map (subtract aveg)
              avg_a = avg a
              avg_b = avg b
              weightedLength average list = lengthVector . map (subtract average) $ list
              (l1,l2) = pearson1 a b

    avg l = sum l / fromIntegral (length l)

