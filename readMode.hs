import System.IO

main = do
 withFile "happy.txt" ReadMode (\handle -> do 
  contents <- hGetContents handle
  putStr contents)