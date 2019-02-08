-- This module synthesizes set functions.
module Synthesize
  ( synthesize, test
  )
where

import           AbstractCurry.Types     hiding ( QName )

import qualified AbstractCurry.Pretty          as ACP
import           FlatCurry.Compact
import           FlatCurry.Annotated.Goodies
import           FlatCurry.Annotated.Pretty
import           FlatCurry.Annotated.Types
import           FlatCurry.Annotated.TypeInference
import           LiftCase
import           Text.PrettyImpl

import           StateMonad
import           Totalize
import           Translate
import           State
import           Utilities
import           Lookup
import           Plural
import           GenNF
import           GenConvertST
import           GenSetFunction

-- Synthesizes set functions for a given module and a list of function names
synthesize :: String -> [QName] -> IO ()
synthesize mod funcs = synth mod funcs >> return ()

synth :: String -> [QName] -> IO ((), State)
synth mod funcs = flip runStateT defaultState $ do
  let funcs' = addMod mod funcs
  p1 <- lift $ computeCompactFlatCurry [InitFuncs funcs'] mod
  modify $ setProg p1
  modify $ setCurrentFunction (head funcs')
  p2 <- lift $ inferProg (totalizeProg p1)
  case p2 of
    Prelude.Left  error -> lift $ putStrLn error
    Prelude.Right p3    -> do
      let p4 = liftCases True p3
      modify $ setProg (unAnnProg p4)
      transformProg funcs'
      genNFInstances
      genConvInstances
      genSetFunction
      p5 <- gets currentCProg
      lift $ putStrLn (showWidth 80 (ACP.ppCurryProg testOptions p5))

-- Functions for quick testing
test :: IO ()
test = synthesize "Examples" [("Examples", "anyOf")]

testOptions :: ACP.Options
testOptions = ACP.setNoQualification ACP.defaultOptions

