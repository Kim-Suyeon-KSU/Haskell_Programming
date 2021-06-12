import System.IO


--입력과 출력이있는 interactive program으로써 State모나드인 IO모나드 사용



getCh :: IO Char
getCh = do hSetEcho stdin False  --입력이 터미널에서 보이지 않도록 함
           x <- getChar
           hSetEcho stdin True
           return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []    --모나드 래핑
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)



play :: String -> IO ()
play word = 
   do putStr "알파벳/단어를 입력하세요: "
      guess <- getLine
      if guess == word then putStrLn "축하합니다! 정답입니다."
      else do putStrLn (match word guess)
              play word


main :: IO ()
main = do putStrLn "단어를 입력하세요: "
          word <- sgetLine    --외부 입력값 저장
          putStrLn "단어를 맞춰보세요!"
          play word

match :: String -> String -> String
match xs ys =
   [if elem x ys then x else '-' | x <- xs]  --검사
