-- This module defines the command line options.
-- Todo: Implement options that are commented out.
module Options where

import           FlatCurry.Types
import           GetOpt
import           ReadNumeric                    ( readNat )
import           List                           ( intercalate
                                                , last
                                                , splitOn
                                                )

data Options = Options
  { optHelp      :: Bool -- Show help?
  , optVerb      :: Int -- Verbosity (0-2)
  , optStore     :: Bool  -- Write result to disk?
  , optFunctions :: [QName]  -- Functions to synthesize set functions of
  }

defaultOptions :: Options
defaultOptions =
  Options {optHelp = False, optVerb = 1, optStore = True, optFunctions = []}

-- Help text
usageText :: String
usageText = usageInfo ("Usage: synsf [options] <module names>\n") options

-- Definition of actual command line options
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?"
           ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  -- , Option "q" ["quiet"] (NoArg (\opts -> opts { optVerb = 0 }))
  --          "run quietly (no output, only exit code)"
  -- , Option "v" ["verbosity"]
  --           (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
  --           "verbosity level:\n0: quiet (same as `-q')\n1: show progress (default)\n2: show generated output (same as `-v')\n3: show more details about translation"
  , Option "f"
           ["function"]
           (ReqArg addFuncName "<n>")
           "name of function for which the set function should be synthesized"
  -- , Option "n" ["nostore"]
  --          (NoArg (\opts -> opts { optStore = False }))
  --          "do not store translation (show only)"
  ]
 where
  -- safeReadNat opttrans s opts =
  --  let numError = error "Illegal number argument (try `-h' for help)" in
  --   maybe numError
  --         (\ (n,rs) -> if null rs then opttrans n opts else numError)
  --         (readNat s)
  -- checkVerb n opts = if n>=0 && n<4
  --                    then opts { optVerb = n }
  --                    else error "Illegal verbosity level (try `-h' for help)"
  addFuncName name opts =
    let
      nameparts = splitOn "." name
      partnums  = length nameparts
      qname     = if partnums < 2
        then ("", name)
        else (intercalate "." (take (partnums - 1) nameparts), last nameparts)
    in
      opts { optFunctions = qname : optFunctions opts }
