-- This module defines the state of the transformation.
module State where

import           AbstractCurry.Types     hiding ( QName )
import           FlatCurry.Types
import           Data.FiniteMap

import           StateMonad
import           Translate

-- The state is a state transformer monad
type OptState m a = StateT State m a

-- While transforming expressions, whenever a new IDSupply or an ID is required,
-- the kind and the variable are added to the state. When the function declaration
-- is generated, for every variable a local declaration is created that either
-- uses or splits the supplied IDSupply appropriately.
data VarKind = VarIDSupply -- Variable is an IDSupply
             | VarID -- Variable is an ID
  deriving Show


-- Maps function names to a list of VarKind - Variable pairs
type SMap = FM QName [(VarKind, CTVarIName)]

-- Maps QNames to QNames
type QMap = FM QName QName

type TList = [(CTypeExpr, CTypeExpr)]

-- State of the transformation
data State = State
  {
    currentProg  :: Prog,
    currentCProg :: CurryProg,
    currentModule  :: String,
    currentFunction :: QName,
    typeMap      :: QMap, -- Maps types to ST types
    typeSTMap    :: QMap, -- Maps ST types to types
    consMap      :: QMap, -- Maps constructors to ST constructors
    funcMap      :: QMap, -- Maps functions to plural functions
    supplyVarMap  :: SMap, -- Maps functions to a list of the occuring ID/IDSupply variables
    funcTypes      :: TList, -- Contains pairs of function type - plural function type pairs
    maxVar       :: VarIndex -- Largest variable ID used in the transformation
  }

defaultState :: State
defaultState = State
  { currentProg     = Prog "" [] [] [] []
  , currentCProg    = CurryProg "" [] Nothing [] [] [] [] []
  , currentModule   = ""
  , currentFunction = ("", "")
  , typeMap         = emptyFM (<)
  , typeSTMap       = emptyFM (<)
  , consMap         = emptyFM (<)
  , funcMap         = emptyFM (<)
  , supplyVarMap    = emptyFM (<)
  , funcTypes       = []
  , maxVar          = 0
  }

setCurrentModule :: String -> State -> State
setCurrentModule m state = state { currentModule = m }

setCurrentFunction :: QName -> State -> State
setCurrentFunction qn state = state { currentFunction = qn }

setTypeMap :: QMap -> State -> State
setTypeMap tmap state = state { typeMap = tmap }

setTypeSTMap :: QMap -> State -> State
setTypeSTMap tstmap state = state { typeSTMap = tstmap }

setConsMap :: QMap -> State -> State
setConsMap cmap state = state { consMap = cmap }

setFuncMap :: QMap -> State -> State
setFuncMap funMap state = state { funcMap = funMap }

addToSupplyVarMap :: QName -> (VarKind, CVarIName) -> State -> State
addToSupplyVarMap qn v state = state { supplyVarMap = smap' }
 where
  smap  = supplyVarMap state
  smap' = case lookupFM smap qn of
    Just _  -> updFM smap qn (v :)
    Nothing -> addToFM smap qn [v]

setFuncTypes :: TList -> State -> State
setFuncTypes funTypes state = state { funcTypes = funTypes }

addnfFDs :: [FuncDecl] -> State -> State
addnfFDs fds' state =
  let (Prog name imps tds fds opds) = currentProg state
  in  state { currentProg = Prog name imps tds (fds ++ fds') opds }

setProg :: Prog -> State -> State
setProg p state = state { currentProg = p }

-- Returns a fresh variable
freshVar :: Monad m => OptState m CVarIName
freshVar = do
  state <- get
  let m  = maxVar state
      m' = m + 1
  put $ state { maxVar = m' }
  return (translVar m')

-- Returns a list of fresh variables
freshVars :: Monad m => Int -> OptState m [CVarIName]
freshVars c = do
  state <- get
  let m  = maxVar state
      ms = [m + 1 .. m + c]
  put $ state { maxVar = m + c }
  return (map translVar ms)

addCurryFDs :: [CFuncDecl] -> State -> State
addCurryFDs fds' state =
  let (CurryProg n is dds cds ids tds fds ods) = currentCProg state
  in  state { currentCProg = CurryProg n is dds cds ids tds (fds ++ fds') ods }

addCurryTDs :: [CTypeDecl] -> State -> State
addCurryTDs tds' state =
  let (CurryProg n is dds cds ids tds fds ods) = currentCProg state
  in  state { currentCProg = CurryProg n is dds cds ids (tds ++ tds') fds ods }

addCurryImports :: [String] -> State -> State
addCurryImports is' state =
  let (CurryProg n is dds cds ids tds fds ods) = currentCProg state
  in  state { currentCProg = CurryProg n (is ++ is') dds cds ids tds fds ods }

addCurryInstances :: [CInstanceDecl] -> State -> State
addCurryInstances ids' state =
  let (CurryProg n is dds cds ids tds fds ods) = currentCProg state
  in  state { currentCProg = CurryProg n is dds cds (ids ++ ids') tds fds ods }
