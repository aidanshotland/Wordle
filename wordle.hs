

import System.IO

wordle :: IO ()
wordle = do 
   putStrLn "Think of a word:"   
   word <- sgetLine    
   putStrLn "Try to guess it:"          
   play word 6

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

play :: String -> Int -> IO ()
play word attemptsLeft = 
   if attemptsLeft == 0 then 
      putStrLn $ "The word was: " ++ word
   else do
      putStr $ "Attempts Left: " ++  show attemptsLeft ++ " "
      guess <- getLine
      if length guess /=5 || not (isValidWord guess) then 
         putStrLn "Invalid Input, please enter a 5 letter word with lowercase letters."
      else do 
         let lettersfound = match word guess 
         putStrLn $ "Correct letters " ++ lettersfound
         if lettersfound ==word && attemptsLeft==1 then 
            putStrLn $ "Phew, you got it!"
         else if lettersfound == word then 
            putStrLn $ "Congrats, you guessed it!"
         else play word (attemptsLeft - 1)
              

match :: String -> String -> String
match xs ys = zipWith (\c1 c2 -> if c1 == c2 then c1 else if c2 `elem` xs then '*' else '-') xs ys



isValidWord :: String -> Bool
isValidWord "" = True
isValidWord (x:xs) = if 'a' <=x && x<= 'z' then True else False


