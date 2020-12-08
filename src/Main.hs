module Main ( main ) where

import System.IO ( Handle, IOMode(ReadMode), withFile )

import System.IO.Strict ( hGetContents )
import System.Directory.Recursive ( getFilesRecursive )
import System.Environment ( getArgs )

import Control.Monad (forM_,  filterM, (>=>), forM )

import Text.ParserCombinators.ReadP ( eof, munch, manyTill, (<++), choice
                                    , skipSpaces, munch1, readP_to_S, ReadP
                                    , string)

import Data.Char ( isSpace )
import Data.List ( isSuffixOf )

import Algebra.Graph ( edges )
import Algebra.Graph.Export.Dot ( exportAsIs )

getModuleContent :: Handle -> IO String
getModuleContent = hGetContents

type ModuleName = String

data ImportDecl = ImportDecl { importName :: !String
                             , qual :: !Bool
                             , qualName :: !(Maybe String)
                             } deriving (Eq, Show)

data ModuleImportDecls = 
  ModuleImportDecls { modulePath :: !String
                    , moduleName_ :: !(Maybe ModuleName)
                    , imports :: ![Maybe ImportDecl]
                    } deriving (Eq, Show)

parseModule :: FilePath -> IO ModuleImportDecls
parseModule filePath = do
  putStrLn $ "Parsing " ++ filePath ++ "..."
  c <- withFile filePath ReadMode getModuleContent
  let res = readP_to_S mainParser c
      result = if null res then (Just ".", []) else fst $ last res
      mname = fst result
      imps = filter (/= Nothing) $ snd result
  forM_ imps (putStrLn . ('\t':) . show)
  return $ ModuleImportDecls filePath mname imps

getFilesRecursive' :: FilePath -> IO [FilePath]
getFilesRecursive' fp = do
  fs <- getFilesRecursive fp
  fs' <- filterM hsOrLhs fs
  putStrLn $ "Found " ++ show (length fs') ++ " files in " ++ fp
  return fs'

main :: IO ()
main = do
  dirs <- getArgs
  results <- forM dirs (getFilesRecursive' >=> mapM parseModule)
  let g = edges $ edges' $ concatMap toEdges $ concat results
  putStrLn $ exportAsIs g

edges' :: [Maybe (String, String)] -> [(String, String)]
edges' [] = []
edges' (Nothing:ps) = edges' ps
edges' (Just p:ps) = p:edges' ps

toEdges :: ModuleImportDecls -> [Maybe (String, String)]
toEdges mid = map (toEdge (moduleName_ mid)) (imports mid)

toEdge :: Maybe ModuleName -> Maybe ImportDecl -> Maybe (String, String)
toEdge _ Nothing = Nothing
toEdge Nothing (Just im) = Just (".", importName im)
toEdge (Just from) (Just im) = Just (from, importName im)

hsOrLhs :: FilePath -> IO Bool
hsOrLhs fp = return $ (".hs" `isSuffixOf` fp) || (".lhs" `isSuffixOf` fp)

importLit :: ReadP String
importLit = string "import"
qualifiedLit :: ReadP String
qualifiedLit = string "qualified"
asLit :: ReadP String
asLit = string "as"
moduleLit :: ReadP String
moduleLit = string "module"

moduleName :: ReadP String
moduleName = munch1 endModuleName

endModuleName :: Char -> Bool
endModuleName c = notWhitespace c || openParen c

notWhitespace :: Char -> Bool
notWhitespace = not . isSpace

openParen :: Char -> Bool
openParen = (== '(')

basicImport :: ReadP (Maybe ImportDecl)
basicImport = do
  _ <- importLit
  _ <- munch1 isSpace
  m <- moduleName
  _ <- munch (/= '\n')
  skipSpaces
  return $ Just (ImportDecl m False Nothing)

qualImport :: ReadP (Maybe ImportDecl)
qualImport = do
  _ <- importLit
  _ <- munch1 isSpace
  _ <- qualifiedLit
  _ <- munch1 isSpace
  m <- moduleName
  _ <- munch (/= '\n')
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
  _ <- munch (/= '\n')
  skipSpaces
  return $ Just (ImportDecl m True (Just q))

skipLine :: ReadP (Maybe a)
skipLine = do
  _ <- munch (/= '\n')
  skipSpaces
  return Nothing

skipLineS :: ReadP String
skipLineS = skipLine >> return ""

importParser :: ReadP (Maybe ImportDecl)
importParser =
  choice [basicImport, qualImport, qualAsImport]

moduleNameParser :: ReadP (Maybe ModuleName)
moduleNameParser = do
  m <- moduleName
  return $ Just m

moduleLit_ :: ReadP ()
moduleLit_ = moduleLit >> return ()

moduleLitOrEof :: ReadP ()
moduleLitOrEof = moduleLit_ <++ eof

mainParser :: ReadP (Maybe ModuleName, [Maybe ImportDecl])
mainParser = do
  _ <- manyTill skipLineS moduleLitOrEof
  _ <- munch1 isSpace
  mn <- moduleNameParser
  _ <- munch (/= '\n')
  skipSpaces
  im <- manyTill (importParser <++ skipLine) eof
  return (mn, im)