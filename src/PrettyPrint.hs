module PrettyPrint (
  Csv,
  getCsvHeader,
  getCsvRow,
  rowToString,
  rowsToString
) where

import Data.List (intercalate)

class Csv a where
  getCsvHeader :: [a] -> String
  getCsvRow :: a -> [String]

rowToString :: [String] -> String
rowToString = intercalate ","

rowsToString :: [String] -> String
rowsToString = intercalate "\n"
