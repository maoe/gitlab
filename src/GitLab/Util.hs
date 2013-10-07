module GitLab.Util
  ( camelToSnake
  ) where

import Data.Char (isUpper, toLower)

camelToSnake :: String -> String
camelToSnake = foldr toSnake "" . lowerHead
  where
    lowerHead [] = []
    lowerHead (c:cs) = toLower c:cs
    toSnake c cs
      | isUpper c = '_':toLower c:cs
      | otherwise = c:cs
