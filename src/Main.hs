module Main where

import System.IO (Handle, IOMode(ReadMode), withFile)
import System.IO.Strict (hGetContents)

import Text.ParserCombinators.ReadP ((<++), (+++), choice, many, skipSpaces, munch1, readP_to_S, ReadP, string)

import Text.Pretty.Simple (pPrint)
import Data.Char (isSpace)
import System.Directory (listDirectory)
import Control.Monad (forM, forM_)

getModuleContent :: Handle -> IO String
getModuleContent = hGetContents

data ImportDecl = ImportDecl { importName :: String
                             , qual :: Bool
                             , qualName :: Maybe String
                             }
                 | NoImportDecl
                 deriving (Eq, Show)

data ModuleImportDecls = ModuleImportDecls { modulePath :: String
                                           , fileName :: String
                                           , imports :: [ImportDecl]
                                           } deriving (Eq, Show)

parseModule :: String -> FilePath -> IO ModuleImportDecls
parseModule fname filePath = do
  c <- withFile filePath ReadMode getModuleContent
  let res = readP_to_S importsParser c
      result = filter (/= NoImportDecl) $ fst $ last res
  return $ ModuleImportDecls filePath fname result

main :: IO ()
main = do
  dir <- listDirectory "./data/"
  results <- forM dir (\fname -> parseModule fname ("./data/" ++ fname))
  pPrint results

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
importsParser = many importOrSkip

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

skipLine :: ReadP ImportDecl
skipLine = do
  _ <- munch1 (/='\n')
  skipSpaces
  return NoImportDecl

importParser :: ReadP ImportDecl
importParser =
  choice [basicImport, qualImport, qualAsImport]

importOrSkip :: ReadP ImportDecl
importOrSkip = importParser <++ skipLine