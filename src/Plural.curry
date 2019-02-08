-- This module defines the plural function transformation.
module Plural
  ( transformProg
  )
where


import           AbstractCurry.Types     hiding ( QName )
import           Data.FiniteMap
import           FlatCurry.Types
import           List                           ( init )

import           State
import           StateMonad
import           Lookup
import           Translate
import           Utilities

-- Adds plural functions for a list of function names
transformProg :: Monad m => [QName] -> OptState m ()
transformProg targets = do
  (Prog name _ _ _ _) <- gets currentProg
  modify $ setCurrentModule name
  fds  <- mapM lookupFuncDecl targets
  cfds <- mapM transformFuncDecl fds
  modify $ addCurryFDs cfds
  modify $ addCurryImports ["ST"]

-- Transforms a type declaration into its ST representation
transformType :: Monad m => TypeDecl -> OptState m ()
transformType (TypeSyn _ _ _ _) = return () -- todo: type synonyms
transformType tyd@(Type qn vis vs cs)
  | isBasicType tyd = do
    tMap   <- gets typeMap
    tSTMap <- gets typeSTMap
    modify $ setTypeMap (addToFM tMap qn qn)
    modify $ setTypeSTMap (addToFM tSTMap qn qn)
    return ()
  | otherwise = do
    qn'    <- renameTypeQName True qn
    tMap   <- gets typeMap
    tSTMap <- gets typeSTMap
    modify $ setTypeMap (addToFM tMap qn qn')
    modify $ setTypeSTMap (addToFM tSTMap qn' qn)
    cs' <- mapM transformConsDecl cs
    let vis' = translVis vis
        vs'  = map translTVar vs
        td   = CType qn' vis' vs' cs' []
    modify $ addCurryTDs [td]

-- Transforms a constructor declaration into its ST representation
transformConsDecl :: Monad m => ConsDecl -> OptState m CConsDecl
transformConsDecl (FlatCurry.Types.Cons qn _ vis tys) = do
  qn'  <- renameConsQName qn
  cMap <- gets consMap
  modify $ setConsMap (addToFM cMap qn qn')
  tys' <- mapM transformTypeExprST tys
  let c    = CContext []
      vis' = translVis vis
  return $ CCons [] c qn' vis' tys' -- todo first argument

-- Transforms a type expression into an ST type expression with
-- an outermost ST constructor
transformTypeExprST :: (Monad m) => TypeExpr -> OptState m CTypeExpr
transformTypeExprST ty = case ty of
  TVar i                      -> return $ addST (CTVar $ translTVar i)
  FuncType d r@(FuncType _ _) -> do
    d' <- transformTypeExpr d
    r' <- transformTypeExprST r
    return $ CFuncType (addST d') r'
  FuncType d r -> do
    d' <- transformTypeExpr d
    r' <- transformTypeExpr r
    return $ CFuncType (addST d') (addST r')
  ForallType _ typ -> transformTypeExprST typ >>= (return . addST)
  TCons      _ _   -> transformTypeExpr ty >>= (return . addST)

-- Transforms a type expression into an ST type expression without
-- an outermost ST constructor
transformTypeExpr :: (Monad m) => TypeExpr -> OptState m CTypeExpr
transformTypeExpr ty = case ty of
  FuncType d r -> do
    d' <- transformTypeExpr d
    r' <- transformTypeExprST r
    return $ CFuncType d' r'
  TCons qn tys -> do
    tMap <- gets typeMap
    case lookupFM tMap qn of
      Just qn' -> do
        tys' <- mapM transformTypeExpr tys
        return $ listToType qn' tys'
      Nothing -> do
        td <- lookupTypeDecl qn
        transformType td
        transformTypeExpr ty
  _ -> return $ translTypeExpr ty

-- Transforms a function into its plural equivalent if necessary
transformFuncQN :: Monad m => QName -> OptState m QName
transformFuncQN = transformQN funcMap

-- Transforms a constructor into its ST equivalent if necessary
transformConsQN :: Monad m => QName -> OptState m QName
transformConsQN = transformQN consMap

transformQN :: Monad m => (State -> QMap) -> QName -> OptState m QName
transformQN f qn = do
  xMap <- gets f
  return $ case lookupFM xMap qn of
    Just qn' -> qn'
    Nothing  -> qn

-- Transforms a branch expression into a branch with a transformed expression and pattern
transformBranchExpr :: Monad m => QName -> BranchExpr -> OptState m CBranchExpr
transformBranchExpr pqn (Branch pat e) = do
  e'   <- transformExpr pqn e
  pat' <- transformPat pat
  return $ cbranch pat' (CSimpleRhs e' [])
 where
  transformPat (Pattern qn is) = do
    qn' <- transformConsQN qn
    return $ CPComb qn' (map (CPVar . translExistingVar) is)
  transformPat (LPattern l) = return $ CPLit (translLit l) -- Transform literal?

-- Transforms a let binding
transformBind :: Monad m => QName -> (VarIndex, Expr) -> OptState m CLocalDecl
transformBind qn (i, e) = do
  ce <- transformExpr qn e
  return $ CLocalPat (CPVar $ translExistingVar i) (CSimpleRhs ce [])

-- Transforms an expression
transformExpr :: Monad m => QName -> Expr -> OptState m CExpr
transformExpr pqn expr = case expr of
  Var i                               -> return $ CVar (translExistingVar i)
  Lit l -> return $ CApply (CSymbol ("ST", "Val")) (CLit (translLit l))
  Comb _     ("Prelude", "failed") _  -> return $ CSymbol ("ST", "Fail")
  Comb ctype qn                    es -> case ctype of
    ConsCall -> do
      qn' <- transformConsQN qn
      ces <- mapM (transformExpr pqn) es
      let t = CApply (CSymbol ("ST", "Val")) (listToExpr qn' ces)
      return t
    FuncCall -> do
      funMap <- gets funcMap
      case lookupFM funMap qn of
        Just qn' -> do
          ces       <- mapM (transformExpr pqn) es
          supplyVar <- freshVar
          modify $ addToSupplyVarMap pqn (VarIDSupply, supplyVar)
          return $ listToExpr qn' (CVar supplyVar : ces)
        Nothing -> do
          fd  <- lookupFuncDecl qn
          cfd <- transformFuncDecl fd
          modify $ addCurryFDs [cfd]
          transformExpr pqn expr
    _ -> notImplemented "transformExpr" "FuncPartCall/ConsPartCall"
  Let binds exp -> do
    exp' <- transformExpr pqn exp
    clds <- mapM (transformBind pqn) binds
    return $ CLetDecl clds exp'
  Free is exp -> do
    exp' <- transformExpr pqn exp
    let clds = map (CLocalVars . (: []) . translExistingVar) is
    return $ CLetDecl clds exp'
  Or e1 e2 -> do
    e1' <- transformExpr pqn e1
    e2' <- transformExpr pqn e2
    id  <- freshVar
    modify $ addToSupplyVarMap pqn (VarID, id)
    let t = listToExpr ("ST", "Choice") [CVar id, e1', e2']
    return t
  Case _ exp brexprs -> do
    exp' <- transformExpr pqn exp
    brs' <- mapM (transformBranchExpr pqn) brexprs
    let ccase   = CCase CRigid (CVar $ translVar 0) brs'
        clambda = CLambda [CPVar $ translVar 0] ccase
    return $ listToExpr ("ST", "applyST") [clambda, exp']
  Typed e ty -> do
    e'  <- transformExpr pqn e
    ty' <- transformTypeExprST ty
    return $ CTyped e' (CQualType (CContext []) ty')

-- Transforms a function declaration into its plural equivalent
transformFuncDecl :: Monad m => FuncDecl -> OptState m CFuncDecl
transformFuncDecl (Func qn ar vis ty r) = do
  qnp    <- renameFuncQName qn
  funMap <- gets funcMap
  let qn' = addQNPostfix "P" qnp
  modify $ setFuncMap (addToFM funMap qn qn')
  typ <- transformTypeExprST ty
  r'  <- transformRule qn r
  let ty'  = addIDSupplyType typ
      vis' = translVis vis
      t    = CQualType (CContext []) ty'
      cfd  = CFunc qn' ar vis' t [r']
  funTypes <- gets funcTypes
  modify $ setFuncTypes ((translTypeExpr ty, typ) : funTypes) -- add type without idsupply arg
  return $ cfd

addIDSupplyType :: CTypeExpr -> CTypeExpr
addIDSupplyType t = CFuncType (CTCons ("ST", "IDSupply")) t

-- Transforms a rule
transformRule :: Monad m => QName -> Rule -> OptState m CRule
transformRule qn (Rule is e) = do
  e'   <- transformExpr qn e
  sMap <- gets supplyVarMap
  sv   <- freshVar
  let pats = map (CPVar . translExistingVar) is
  case lookupFM sMap qn of
    Just xs -> do
      ldcls <- genIDSupplyLDecls xs sv
      let r = CRule (CPVar sv : pats) (CSimpleRhs e' ldcls)
      return r
    Nothing -> return $ CRule pats (CSimpleRhs e' [])
transformRule _ (External _) = notImplemented "transformRule" "External rules"

-- While transforming expressions, whenever a new IDSupply or an ID is required,
-- the kind and the variable are added to the state. When the function declaration
-- is generated, for every variable a local declaration is created that either
-- uses or splits the supplied IDSupply appropriately.
genIDSupplyLDecls
  :: Monad m
  => [(VarKind, CVarIName)] -- Pairs of variables and their kinds
  -> CVarIName -- Initial IDSupply
  -> OptState m [CLocalDecl]
genIDSupplyLDecls xs var = do
  vs <- freshVars (length xs)
  let xvs    = zip3 xs vs (var : vs)
      ldecls = init $ concatMap genIDSupplyLDeclPair xvs
  return ldecls

-- Generates local declarations for a variable, a new IDSupply variable for the next
-- declaration and a IDSupply variable that is used to create new IDSupplies.
genIDSupplyLDeclPair
  :: ((VarKind, CVarIName), CVarIName, CVarIName) -> [CLocalDecl]
genIDSupplyLDeclPair ((kind, x), v, s) =
  let rhs n e = CSimpleRhs (CApply (CSymbol ("ST", n)) e) []
      ldecl n var = CLocalPat (CPVar var) (rhs n (CVar s))
      fname = case kind of
        VarID       -> "uniqueID"
        VarIDSupply -> "rightSupply"
  in  [ldecl fname x, ldecl "leftSupply" v]

rename :: Monad m => (String -> String) -> Bool -> QName -> OptState m QName
rename rf st (_, n) = do
  m <- gets currentModule
  let qn = (m, rf n)
  return $ if st then addQNPrefix "ST" qn else qn

-- Renames a type, optionally with an ST prefix
renameTypeQName :: Monad m => Bool -> QName -> OptState m QName
renameTypeQName = rename renameType

-- Renames a function or constructor
renameFuncQName, renameConsQName :: Monad m => QName -> OptState m QName
renameFuncQName = rename renameFunc False
renameConsQName = rename renameCons True
