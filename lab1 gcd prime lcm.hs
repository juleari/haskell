test t@(x, y) =  print x
              >> print y
              >> print t

mygcd :: Int -> Int -> Int
mygcd a b   | a < b      = mygcd b a
            | r == 0     = b
            | otherwise  = mygcd b r
            where r = a `mod` b

mylcm :: Int -> Int -> Int
mylcm a b = div (abs (a * b)) (mygcd a b)

primediv a b    | b ^ 2 > a    = True
                | mod a b == 0 = False
                | otherwise    = primediv a (b + 1)

prime :: Int -> Bool
prime a = primediv a 2

main = print (mylcm 12 16)