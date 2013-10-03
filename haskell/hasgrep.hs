import Control.Monad

main = do 
    contents <- getContents
    let numLines = zipWith (\a b -> (show a) ++ ". " ++ b) [0..] $ lines contents
    putStr $ unlines numLines
