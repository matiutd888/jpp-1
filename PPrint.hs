module PPrint where
import Data.Foldable (Foldable(foldl'))

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = showString k . showString ": " . (show v ++)

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showString "\n"
pprH = intercalateS $ showString " "

intercalateS :: ShowS -> [ShowS] -> ShowS
-- intercalateS sep [] = id
intercalateS sep = foldr (\s acc -> s . sep . acc) id

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith =  (foldl' (.) id .) . map

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
