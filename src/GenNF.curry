-- This module generates normal form instances.
module GenNF
  ( genNFInstances
  )
where

import           AbstractCurry.Types

import           State
import           StateMonad
import           Utilities

-- Generates normal form instances for all generated
-- ST data types.
genNFInstances :: Monad m => OptState m ()
genNFInstances = do
  (CurryProg _ _ _ _ _ tydecls _ _) <- gets currentCProg
  mapM_ transformed tydecls
 where
  transformed t@(CType (_, n) _ _ _ _) = case n of
    'S' : 'T' : _ -> genNF t
    _             -> return ()
  transformed (CTypeSyn _ _ _ _  ) = return ()
  transformed (CNewType _ _ _ _ _) = return ()

-- Generates normal form instance for a type declaration.
genNF :: Monad m => CTypeDecl -> OptState m ()
genNF (  CTypeSyn _ _ _ _     ) = return ()
genNF (  CNewType _  _ _   _ _) = return ()
genNF t@(CType    qn _ tvs _ _) = do
  x   <- freshVar
  brs <- genNFBranches t
  let context = CContext (map ((,) ("ST", "NF") . CTVar) tvs)
      ty      = listToType qn (map CTVar tvs)
      vis     = Private
      nfFunc  = CFunc ("Test", "nf") 1 vis (CQualType context ty) [rule]
      rule    = CRule [CPVar x] rhs
      rhs     = CSimpleRhs exp []
      exp     = CCase CRigid (CVar x) brs
      inst    = CInstance ("ST", "NF") context ty [nfFunc]
  modify $ addCurryInstances [inst]

genNFBranches :: Monad m => CTypeDecl -> OptState m [CBranchExpr]
genNFBranches (CTypeSyn _ _ _ _    ) = return []
genNFBranches (CNewType _ _ _ _   _) = return []
genNFBranches (CType    _ _ _ cds _) = do
  mapM cdToBr cds
 where
  cdToBr (CRecord _ _ _ _ _) = notImplemented "genNFBranches" "Record types"
  cdToBr (CCons _ _ cqn _ tys)
    | length tys == 0
    = let exp = CApply (CSymbol ("ST", "Val")) (CSymbol cqn)
      in  return $ cbranch (CPComb cqn []) (CSimpleRhs exp [])
    | otherwise
    = do
      cvs      <- freshVars (length tys)
      caseExpr <- nfSTCase cqn cvs []
      let pats = map CPVar cvs
      return $ cbranch (CPComb cqn pats) (CSimpleRhs caseExpr [])

-- Generates the normal form expression of a case expression
nfSTCase
  :: Monad m
  => QName -- Name of the constructor
  -> [CVarIName] -- Remaining variables that occur in the constructor pattern
  -> [CVarIName] -- Variables that have been bound by variable patterns
  -> OptState m CExpr
nfSTCase cqn [] vs =
  return $ CApply (CSymbol ("ST", "Val")) (listToExpr cqn (map CVar vs))
nfSTCase cqn (x : cvs) vs = do
  choiceBranch <- genNFChoiceBranch cqn cvs vs
  dcBranch     <- gen_Branch cqn cvs vs
  let nfSTe      = CApply (CSymbol ("ST", "nfST")) (CVar x)
      failBranch = genNFFailBranch
  return $ CCase CRigid nfSTe [choiceBranch, failBranch, dcBranch]

-- Generates a branch for a choice pattern
genNFChoiceBranch
  :: Monad m
  => QName -- Name of the constructor
  -> [CVarIName] -- Remaining variables that occir in the constructor pattern
  -> [CVarIName] -- Variables that have been bound by variable patterns
  -> OptState m CBranchExpr
genNFChoiceBranch cqn cvs vs = do
  is@[id, c1, c2] <- freshVars 3
  let choice es = listToExpr ("ST", "Choice") es
      nf es = listToExpr ("ST", "nf") es
      ces = map CVar vs
      nfArg v = [listToExpr cqn (ces ++ CVar v : (map CVar cvs))]
      choiceExp = choice [CVar id, nf $ nfArg c1, nf $ nfArg c2]
      choiceRhs = CSimpleRhs choiceExp []
  return $ cbranch (CPComb ("ST", "Choice") (map CPVar is)) choiceRhs

-- Generates a branch for the fail pattern
genNFFailBranch :: CBranchExpr
genNFFailBranch =
  let failQN = ("ST", "Fail")
  in  cbranch (CPComb failQN []) (CSimpleRhs (CSymbol failQN) [])

-- Generates a branch for the variable pattern
gen_Branch
  :: Monad m => QName -> [CVarIName] -> [CVarIName] -> OptState m CBranchExpr
gen_Branch cqn cvs vs = do
  x    <- freshVar
  expr <- nfSTCase cqn cvs (vs ++ [x])
  return $ cbranch (CPVar x) (CSimpleRhs expr [])
