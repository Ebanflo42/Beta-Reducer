module Main where

import Expression

betaReduceStr :: String -> String
betaReduceStr = show . betaReduce . parse

loop str =
  case str of
    "q" -> return ()
    _   -> putStrLn (betaReduceStr str) >> getLine >>= loop

main :: IO ()
main = do
  putStrLn "\'q\' to quit"
  getLine >>= loop