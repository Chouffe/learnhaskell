module Test where


sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello " ++ x ++ "!")

triple :: Num a => a -> a
triple x = x * 3

circleArea :: Floating a => a -> a
circleArea r = pi * r * r

x :: IO ()
x = putChar 'a'
