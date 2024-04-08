module StringOPermute where

stringOPermute :: String -> String
stringOPermute "" = ""
stringOPermute [x] = [x]
stringOPermute (x:y:xs) = y : x : stringOPermute xs