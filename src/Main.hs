module Main where

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main :: IO ()
main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needed"
    myFunction = fixLines

fixLines :: String -> String
fixLines input = unlines (splitLines input)

splitLines :: [Char] -> [[Char]]
splitLines [] = []
splitLines xs =
  let (pre, suf) = break isLineTerminator xs
   in pre :
      case suf of
        ('\r':'\n':rest) -> splitLines rest
        ('\n':rest) -> splitLines rest
        ('\r':rest) -> splitLines rest
        _ -> []

isLineTerminator c = c == '\r' || c == '\n'
