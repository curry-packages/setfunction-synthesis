module Main where

import           FlatCurry.Types
import           GetOpt
import           System.CurryPath         ( stripCurrySuffix )
import           System

import           Options
import           Synthesize

main :: IO ()
main = do
  argv <- getArgs
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts                       = foldl (flip id) defaultOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> putStrLn usageText >> exitWith 1)
  when (null args || optHelp opts) (putStrLn usageText >> exitWith 1)
  synthesize (head $ map stripCurrySuffix args) (optFunctions opts)
