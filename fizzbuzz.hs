module Main where
    main = print $ map fizz [1..100]

    fizz :: Int -> String
    fizz n 
      | mod n 15 == 0 = "FizzBuzz"
      | mod n 3 == 0 = "Fizz"
      | mod n 5 == 0 = "Buzz"
      | otherwise = show n
    

