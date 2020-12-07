module Module3 ( f1, f2 ) where

f1 :: IO Int
f1 = return 1

f2 :: IO ()
f2 = print "Hello!"

main :: IO ()
main = f2