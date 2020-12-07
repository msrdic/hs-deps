module Main (main) where

import System.IO (Handle, IOMode(ReadMode), withFile)
import System.IO.Strict (hGetContents)
import System.Directory.Recursive ( getFilesRecursive )
import System.Environment (getArgs)
import System.FilePath.Posix ( splitPath )

import Control.Monad (filterM, (>=>), forM)

import Text.ParserCombinators.ReadP ((<++), choice, many, skipSpaces, munch1, readP_to_S, ReadP, string)
import qualified Text.Pretty.Simple as PP

import Data.Char (isSpace)
import Data.List (isSuffixOf)

getModuleContent :: Handle -> IO String
getModuleContent = hGetContents

data ImportDecl = ImportDecl { importName :: String
                             , qual :: Bool
                             , qualName :: Maybe String
                             } deriving (Eq, Show)

data ModuleImportDecls = 
  ModuleImportDecls { modulePath :: String
                    , fileName :: String
                    , imports :: [Maybe ImportDecl]
                    } deriving (Eq, Show)

parseModule :: FilePath -> IO ModuleImportDecls
parseModule filePath = do
  PP.pPrint filePath
  c <- withFile filePath ReadMode getModuleContent
  let res = readP_to_S importsParser c
      result = filter (/= Nothing) $ fst $ last res
      fname = last $ splitPath filePath
  return $ ModuleImportDecls filePath fname result

main :: IO ()
main = do
  dirs <- getArgs
  PP.pPrint dirs
  results <- forM dirs (getFilesRecursive >=> filterM hsOrLhs >=> mapM parseModule)
  PP.pPrint results

hsOrLhs :: FilePath -> IO Bool
hsOrLhs fp = return $ (".hs" `isSuffixOf` fp) || (".lhs" `isSuffixOf` fp)

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

importsParser :: ReadP [Maybe ImportDecl]
importsParser = many importOrSkip

basicImport :: ReadP (Maybe ImportDecl)
basicImport = do
  _ <- importLit
  _ <- munch1 isSpace
  m <- moduleName
  _ <- munch1 isSpace
  return $ Just (ImportDecl m False Nothing)

qualImport :: ReadP (Maybe ImportDecl)
qualImport = do
  _ <- importLit
  _ <- munch1 isSpace
  _ <- qualifiedLit
  _ <- munch1 isSpace
  m <- moduleName
  skipSpaces
  return $ Just (ImportDecl m True Nothing)

qualAsImport :: ReadP (Maybe ImportDecl)
qualAsImport = do
  _ <- importLit
  _ <- munch1 isSpace
  _ <- qualifiedLit
  _ <- munch1 isSpace
  m <- moduleName
  _ <- munch1 isSpace
  _ <- asLit
  _ <- munch1 isSpace
  q <- moduleName
  skipSpaces
  return $ Just (ImportDecl m True (Just q))

skipLine :: ReadP (Maybe ImportDecl)
skipLine = do
  _ <- munch1 (/='\n')
  skipSpaces
  return Nothing

importParser :: ReadP (Maybe ImportDecl)
importParser =
  choice [basicImport, qualImport, qualAsImport]

importOrSkip :: ReadP (Maybe ImportDecl)
importOrSkip = importParser <++ skipLine