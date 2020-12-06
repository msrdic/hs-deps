module Main where

import System.IO (Handle, IOMode(ReadMode), withFile)
import System.IO.Strict (hGetContents)

filePath :: FilePath
filePath = "./src/Main.hs"

content :: Handle -> IO String
content = hGetContents

main :: IO ()
main = do
  c <- withFile filePath ReadMode content
  print c