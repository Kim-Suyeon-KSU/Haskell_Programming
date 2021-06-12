import System.IO

--입력과 출력이있는 interactive program으로써 State모나드인 IO모나드 사용

main :: IO ()
main = do putStrLn "단어를 입력하세요: "
          word <- sgetLine
          putStrLn "단어를 맞춰보세요!"
          play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x


play :: String -> IO ()
play word = 
   do putStr "알파벳/단어를 입력하세요: "
      guess <- getLine
      if guess == word then putStrLn "축하합니다! 정답입니다."
      else do putStrLn (match word guess)
              play word


match :: String -> String -> String
match xs ys =
   [if elem x ys then x else '-' | x <- xs]
