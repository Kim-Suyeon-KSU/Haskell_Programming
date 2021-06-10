import System.IO
import Data.Char

main = do
 contents <- readFile "happy.txt"
 writeFile "happycaps.txt" (map toUpper contents)