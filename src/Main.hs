module Main ( main ) where

import qualified Language.Haskell.Exts as P


import System.Directory.Recursive ( getFilesRecursive )
import System.Environment ( getArgs )

import Control.Monad (filterM, forM )


import Data.List ( isSuffixOf )

import Algebra.Graph ( edges )
import Algebra.Graph.Export.Dot ( exportAsIs )

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

getFilesRecursive' :: FilePath -> IO [FilePath]
getFilesRecursive' fp = do
  fs <- getFilesRecursive fp
  fs' <- filterM hsOrLhs fs
  putStrLn $ "#Found " ++ show (length fs') ++ " files in " ++ fp
  return fs'

main :: IO ()
main = do
  dirs <- getArgs
  results <- forM dirs getFilesRecursive'
  let files = concat results
  parseResults <- mapM P.parseFile files
  let g = edges $ edges' $ concatMap toEdges parseResults
  putStrLn $ exportAsIs g

edges' :: [Maybe (String, String)] -> [(String, String)]
edges' [] = []
edges' (Nothing:ps) = edges' ps
edges' (Just p:ps) = p:edges' ps

toEdges :: P.ParseResult (P.Module P.SrcSpanInfo) -> [Maybe (String, String)]
toEdges (P.ParseOk (P.Module _ (Just (P.ModuleHead _ (P.ModuleName _ mname) _ _)) _ impdecs _)) =
  map (toEdge (Just mname)) impdecs
toEdges _ = []

toEdge :: Maybe ModuleName -> P.ImportDecl a -> Maybe (String, String)
toEdge (Just from) (P.ImportDecl _ (P.ModuleName _ mname) _ _ _ _ _ _) = Just (from, mname)
toEdge _ _ = Nothing

hsOrLhs :: FilePath -> IO Bool
hsOrLhs fp = return $ (".hs" `isSuffixOf` fp) || (".lhs" `isSuffixOf` fp)