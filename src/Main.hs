-- Antonio Curci
-- Mat. 761049
-- CurcIMP, FMCS 2021-2022

module Main where
import System.IO
import Stack
import Interpreter
import Grammar
import Parser

main :: IO ()
main = do
      putStrLn . unlines $ map concatNums choices
      choice <- getLine
      case validate choice of
         Just n  -> execute . read $ choice           -- when a line is correctly inserted the choice is evaluated
         Nothing -> putStrLn "Please try again"       -- when line is not inserted correctly

      main
   where concatNums (i, (s, _)) = show i ++ ".) " ++ s   -- displaying the choices in a certain string format

validate :: String -> Maybe Int                          -- This function validates the inserted choice, if it's valid or not. 
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, IO ()))]                     -- Display of the choices
choices = zip [1, 2, 3] [
   ("Run a program from a file", foo)
 , ("Write your program in a line", bar)
 , ("Exit", esc )
 ]

execute :: Int -> IO ()                                 -- executes the main
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f

-- Here i implement the body for each of the choices of the menu

-- First option

foo =                                               
  do     
  putStrLn "Enter the name of the file you want to run:"
  input <- getLine; 
  p <- readFile input
  let c = parse p
  if parseFailed c
    then do
      putStrLn "\nParsing failed\n"
      putStrLn "\nRemaining input:\n"
      print (getRemainingInput c)
  else do
      putStrLn "\nParsing success!\n"
      let s = execProgr [] (getParsedCommands c)
      putStrLn "\nInput Program\n"
      putStrLn p
      putStrLn "\nRepresentation of the program:\n"
      print (getParsedCommands c)
      putStrLn "\nState of the memory:\n"
      print s


-- Second option
bar = 
  do
  putStrLn "Write a program all in one line:"
  string <- getLine;
  let c = parse string
  if parseFailed c
     then do
      putStr "\nParsing failed\n"
      putStr "\nRemaining input:"
      print (getRemainingInput c)
  else do
      putStrLn "\nParsing success!\n"
      let s = execProgr [] (getParsedCommands c)
      putStr "\nInput Program\n"
      putStr string
      putStr "\nRepresentation of the program:\n"
      print (getParsedCommands c)
      putStr "\nState of the memory:\n"
      print s 

-- Third option
esc = 
  do 
    error "Exit from the program! Goodbye!"
      