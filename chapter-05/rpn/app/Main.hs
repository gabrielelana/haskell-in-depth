module Main (main) where

import System.Environment
import Lib

main :: IO ()
main = do
  (e:_) <- getArgs
  case parseExp e of
    Left er -> putStrLn $ "Parse Error in " ++ show er
    Right ex -> putStrLn $ "Expression: " ++ show ex ++ "\nValue: " ++ show (evalExp ex)
