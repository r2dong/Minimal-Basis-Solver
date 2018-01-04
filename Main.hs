import Grammar
import Tokens
import System.Environment
import Data.List.Split
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C
import MinimalBasis

process :: FDs -> String
process (FDs attr fd) = allToString attr (findMinimalBasis fd)

attrToString :: [Attr]  -> String
attrToString [] = []
attrToString (x : []) = C.unpack x
attrToString (x : xs) = (C.unpack x) ++ " , " ++ attrToString xs

fdToString :: FD -> String
fdToString (lhs, rhs) = (attrToString lhs) ++ " -> " ++ (attrToString rhs) ++ " ; \n"

fdsToString :: [FD] -> String
fdsToString [] = []
fdsToString (x : xs) = (fdToString x) ++ fdsToString xs

allToString :: [Attr] -> [FD] -> String
allToString attr fd = "Relation: \n" ++ (attrToString attr) ++ "\nFDs: \n" ++ (fdsToString fd)

writeToFiles :: String -> String -> IO ()
writeToFiles name contents  =
  writeFile (name  ++ ".txt") contents

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  let filename = head (splitOn "." (head args)) ++ "Out"
  writeToFiles filename $ process $ parseFDs $ scanTokens s
