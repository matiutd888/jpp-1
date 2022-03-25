module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k, v) = showString k . showString ": " . (show v ++)

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showString "\n"
pprH = intercalateS $ showString " "

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep [] = id
intercalateS sep (x : xs) = x . foldr (.) id (map (sep .) xs)

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith x y = intercalateS (showString "\n") $ map x y

runShows :: ShowS -> IO ()
runShows = putStrLn . ($ "")
