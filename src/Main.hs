module Main where

import System.IO (Handle, IOMode(ReadMode), withFile)
import System.IO.Strict (hGetContents)

import Text.ParserCombinators.ReadP (choice, many, skipSpaces, munch1, readP_to_S, ReadP, string)

import Text.Pretty.Simple (pPrint)
import Data.Char (isSpace)

filePath :: FilePath
filePath = "./data/Import.hs"

getModuleContent :: Handle -> IO String
getModuleContent = hGetContents

data ImportDecl = ImportDecl { importName :: String
                             , qual :: Bool
                             , qualName :: Maybe String
                             } deriving (Eq, Show)

main :: IO ()
main = do
  c <- withFile filePath ReadMode getModuleContent
  let res = readP_to_S importsParser c
      result = fst $ last res
  pPrint result

moduleDeclLiteral :: ReadP String
moduleDeclLiteral = string "module"

importLit :: ReadP String
importLit = string "import"
qualifiedLit :: ReadP String
qualifiedLit = string "qualified"
asLit :: ReadP String
asLit = string "as"

moduleName :: ReadP String
moduleName = munch1 endModuleName

endModuleName :: Char -> Bool
endModuleName c = notWhitespace c || openParen c

notWhitespace :: Char -> Bool
notWhitespace = not . isSpace

openParen :: Char -> Bool
openParen = (== '(')
closeParen :: Char -> Bool
closeParen = (== '(')

importsParser :: ReadP [ImportDecl]
importsParser = many importParser

basicImport :: ReadP ImportDecl
basicImport = do
  importLit
  skipSpaces
  m <- moduleName
  skipSpaces
  return $ ImportDecl m False Nothing

qualImport :: ReadP ImportDecl
qualImport = do
  importLit
  skipSpaces
  qualifiedLit
  skipSpaces
  m <- moduleName
  skipSpaces
  return $ ImportDecl m True Nothing

qualAsImport :: ReadP ImportDecl
qualAsImport = do
  importLit
  skipSpaces
  qualifiedLit
  skipSpaces
  m <- moduleName
  skipSpaces
  asLit
  skipSpaces
  q <- moduleName
  skipSpaces
  return $ ImportDecl m True (Just q)

importParser :: ReadP ImportDecl
importParser =
  choice [basicImport, qualImport, qualAsImport]

parser :: ReadP String
parser = do
  _ <- moduleDeclLiteral
  _ <- skipSpaces
  mname <- moduleName
  _ <- skipSpaces
  return mname