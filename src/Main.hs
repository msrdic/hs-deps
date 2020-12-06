module Main where

import System.IO (Handle, IOMode(ReadMode), withFile)
import System.IO.Strict (hGetContents)

filePath :: FilePath
filePath = "./src/Main.hs"

getModuleContent :: Handle -> IO String
getModuleContent = hGetContents

main :: IO ()
main = do
  c <- withFile filePath ReadMode getModuleContent
  print c