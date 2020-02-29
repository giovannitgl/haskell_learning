module Pangram (isPangram) where
import Data.Char

strToLow :: String -> String
strToLow str = [toLower lStr | lStr <- str]

pangramPreProcess :: String -> String
pangramPreProcess text = [x | x <- strToLow text, x `elem` ['a'..'z']]

isPangramAux :: String -> String -> Bool
isPangramAux text (c:cs) = if not contains then False
                        else isPangramAux text cs
                        where contains = c `elem` text
isPangramAux _ [] = True

isPangram :: String -> Bool
isPangram "" = False
isPangram text = isPangramAux processed ['a'..'z']
                where processed = pangramPreProcess text

