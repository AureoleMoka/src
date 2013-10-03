import Data.List

main = putStrLn . show $ filter nombreParfait [1..8128]

fastdivs :: (Integral a) => a -> [a]
fastdivs x = sort $ div1 ++ div2
    where sqrx = ceiling . sqrt $ fromIntegral x
          div1 = [a | y <- [1..sqrx]
                 , mod x y == 0
                 , let a = div x y
                 ]
          div2 = [y | y <- [1..sqrx]
                  , mod x y == 0
                  , not $ elem y div1
                  ]

nombrePremier :: (Integral a) => a -> Bool
nombrePremier x = take 2 (fastdivs x)
                == [1,x]
                
nombreParfait :: (Integral a, Show a) => a -> Bool
nombreParfait x = and 
                [ or
                    [ (read [last . show $ x] :: Int) == 6
                    , (read [last . show $ x] :: Int) == 8
                    ]
                , not $ nombrePremier x
                , sum (init (fastdivs x)) == x 
                ]

nP' :: Int -> Int
nP' x = (2^(x-1))*(2^x-1)
