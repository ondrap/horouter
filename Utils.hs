module Utils (
    camelTo_
) where
    
import Data.Char (toLower, isUpper)

camelTo_ :: String -> String
camelTo_ = map toLower . drop 1 . reverse . foldl insertUnderscore []
    where
        insertUnderscore acc chr
            | isUpper chr = chr : '_' : acc
            | otherwise   = chr : acc
                
