-- This module generates ConvertST instances like toValST
-- and fromValST as well as the functions toST and fromST.
module GenConvertST
  ( genConvInstances
  )
where

import           AbstractCurry.Types     hiding ( QName )
import           FlatCurry.Types         hiding ( Visibility(..) )
import           List                           ( nub )
import           Data.FiniteMap

import           State
import           StateMonad
import           Utilities
import           Translate
import           Lookup

-- Generates CovertST instances based on the information
-- added to the state by the plural transformation.
genConvInstances :: Monad m => OptState m ()
genConvInstances = do
  ftys <- gets funcTypes
  let qns = nub $ concatMap compareTypes ftys
  let lookupT (q1, q2) = do
        td1 <- lookupTypeDecl q1
        td2 <- lookupCTypeDecl q2
        case td2 of
          Prelude.Left  td  -> return (td1, translTypeDecl td)
          Prelude.Right ctd -> return (td1, ctd)
  tds <- mapM lookupT qns
  mapM_ (uncurry genConv) tds
  return ()

-- Generates ConvertST instances for a pair of types where
-- one is a FlatCurry type (from the original program) and
-- the other is a generated AbstractCurry data type.
genConv :: Monad m => TypeDecl -> CTypeDecl -> OptState m ()
genConv t1 t2 = case (t1, t2) of
  (Type qn1 _ _ _, CType qn2 _ _ _ _) ->
    if qn1 == qn2 then genBasic qn1 else genComplex t1 t2
  _ -> return ()

-- Generates a basic ConvertST instances for a data type.
-- Basic instances "convert" between the same types,
-- for example toValST_Int_Int
genBasic :: Monad m => QName -> OptState m ()
genBasic qn@(m, n) = do
  let qn' s = (m, s ++ "_" ++ n ++ "_" ++ n)
      typ f1 f2 =
        CQualType (CContext []) (CFuncType (f1 $ CTCons qn) (f2 $ CTCons qn))
      rule = CRule [] (CSimpleRhs (CSymbol ("Prelude", "id")) [])
      func qname = CFunc (qn' qname) 1 Public (typ id id) [rule]
  toSTRule   <- genRuleToST 0 (qn' "toValST")
  fromSTRule <- genRuleFromST 0 (qn' "fromValST")
  let func' f1 f2 qname r = CFunc (qn' qname) 1 Public (typ f1 f2) [r]
      funcs =
        [ func "toValST"
        , func "fromValST"
        , func' id    addST     "toST"   toSTRule
        , func' addST addValues "fromST" fromSTRule
        ]
  modify $ addCurryFDs funcs

genComplex :: Monad m => TypeDecl -> CTypeDecl -> OptState m ()
genComplex t1 t2 = case (t1, t2) of
  (Type qn1 _ tvs1 cds1, CType qn2 _ _ cds2 _) -> do
    let vcnt    = length tvs1
        valFunc = genFunc vcnt cds1 cds2 (qn1, qn2)
    toValSTFunc   <- valFunc ToValST
    fromValSTFunc <- valFunc FromValST
    toSTFunc      <- valFunc ToST
    fromSTFunc    <- valFunc FromST
    modify $ addCurryFDs [toValSTFunc, fromValSTFunc, toSTFunc, fromSTFunc]
  _ -> return ()

-- Represents the different ConvertST instances/functions
data STFunction = ToValST | FromValST | ToST | FromST

-- Generates a ConvertST instance or function
genFunc
  :: Monad m
  => Int -- Number of type variables
  -> [ConsDecl] -- Constructors of the FlatCurry type
  -> [CConsDecl] -- Constructors of the AbstractCurry type
  -> (QName, QName) -- Names of the types
  -> STFunction -- Which function to generate
  -> OptState m CFuncDecl
genFunc vcnt cds1 cds2 (qn1@(m, n1), qn2@(_, n2)) stf =
  let
    qn' = genQName m n1 n2
    eis = take vcnt evens
    ois = take vcnt odds
    funcType st cty1 cty2 cnstrs = CQualType
      (CContext cnstrs)
      (foldr CFuncType cty2 (argFuncTypes st ++ [cty1]))
    argFuncTypes st = map (argFuncType st) eis
    consType qn f is = f $ listToType qn (map (CTVar . translTVar) is)
    argFuncType f i =
      CFuncType (CTVar $ translTVar i) (f (CTVar . translTVar $ i + 1))
    genRules f insts = mapM (uncurry $ f vcnt insts) (zip cds1 cds2)
    funcDecl qn typ rules = CFunc qn (vcnt + 1) Public typ rules
  in
    case stf of
      ToValST -> do
        ftys  <- gets funcTypes
        rules <- genRules genRuleToValST (nub $ concatMap compareTypes ftys)
        let typ = funcType addST (consType qn1 id eis) (consType qn2 id ois) []
        return $ funcDecl (qn' "toValST") typ rules
      FromValST -> do
        ftys  <- gets funcTypes
        rules <- genRules genRuleFromValST
                          (map swap $ nub (concatMap compareTypes ftys))
        let typ = funcType id (consType qn2 id eis) (consType qn1 id ois) []
        return $ funcDecl (qn' "fromValST") typ rules
      ToST -> do
        rule <- genRuleToST vcnt (qn' "toValST")
        let typ =
              funcType addST (consType qn1 id eis) (consType qn2 addST ois) []
        return $ funcDecl (qn' "toST") typ [rule]
      FromST -> do
        rule   <- genRuleFromST vcnt (qn' "fromValST")
        tSTMap <- gets typeSTMap
        case lookupFM tSTMap qn2 of
          Just oqn ->
            let
              typ = funcType id
                             (consType qn2 addST eis)
                             (consType oqn addValues ois)
                             cnstrs
              cnstrs = map (\i -> (("ST", "NF"), CTVar $ translTVar i)) eis
            in
              return $ funcDecl (qn' "fromST") typ [rule]
          Nothing ->
            error $ "genFunc: Missing original data type for " ++ show qn2

genRuleToValST
  :: Monad m
  => Int -- Number of type variables
  -> [(QName, QName)] -- Pairs of type - ST type names
  -> ConsDecl
  -> CConsDecl
  -> OptState m CRule
genRuleToValST _ _ (FlatCurry.Types.Cons _ _ _ _) (CRecord _ _ _ _ _) =
  notImplemented "genRuleToValSt" "Record types"
genRuleToValST tvars insts (FlatCurry.Types.Cons qn ar _ tys) (CCons _ _ cqn _ _)
  = do
    m  <- gets currentModule
    vs <- freshVars tvars
    xs <- freshVars ar
    let iargs      = zip (nub $ concatMap typeVars tys) vs
        txs        = zip xs tys
        rule       = CRule (map CPVar vs ++ [CPComb qn (map CPVar xs)]) rhs
        genRuleExp = genRuleToValSTExpr m iargs insts
        rhs        = CSimpleRhs (listToExpr cqn (map genRuleExp txs)) []
    return rule

genRuleFromValST
  :: Monad m
  => Int -- Number of type variables
  -> [(QName, QName)] -- Pairs of type - ST type names
  -> ConsDecl
  -> CConsDecl
  -> OptState m CRule
genRuleFromValST _ _ (FlatCurry.Types.Cons _ _ _ _) (CRecord _ _ _ _ _) =
  notImplemented "genRuleFromValST" "Record types"
genRuleFromValST tvars insts (FlatCurry.Types.Cons qn ar _ _) (CCons _ _ cqn _ ctys)
  = do
    m  <- gets currentModule
    vs <- freshVars tvars
    xs <- freshVars ar
    let iargs = zip (nub $ concatMap ctypeVars ctys) vs
        txs   = zip xs ctys
        pat i = CPComb ("ST", "Val") [CPVar i]
        rule       = CRule (map CPVar vs ++ [CPComb cqn (map pat xs)]) rhs
        genRuleExp = genRuleFromValSTExpr m iargs insts
        rhs        = CSimpleRhs (listToExpr qn (map genRuleExp txs)) []
    return rule

genRuleToValSTExpr
  :: String -- Current module
  -> [(TVarIndex, CTVarIName)] -- Pairs of type variable - argument function variable
  -> [(QName, QName)] -- Pairs of type - ST type names
  -> (CTVarIName, TypeExpr) -- Pattern variable and its type
  -> CExpr
genRuleToValSTExpr m iargs insts (y, t) = CApply (genRuleExpr' t) (CVar y)
 where
  genRuleExpr' typ = case typ of
    TVar i -> case lookup i iargs of
      Just v -> CVar v
      Nothing ->
        error
          $  "genRuleToValSTExpr: Missing instance argument for variable "
          ++ show i
    FuncType _ _ ->
      notImplemented "genRuleToValSTExpr" "Higher-order functions"
    TCons qname@(_, n1) ts -> case lookup qname insts of
      Just (_, n2) ->
        let args   = map genRuleExpr' ts
            qname' = genQName m n1 n2 "toST"
        in  listToExpr qname' args
      Nothing ->
        error $ "genRuleToValSTExpr: Missing instance for " ++ show qname
    ForallType _ ty -> genRuleExpr' ty

genRuleFromValSTExpr
  :: String -- Current module
  -> [(CTVarIName, CTVarIName)] -- Pairs of type variable - argument function variable
  -> [(QName, QName)] -- Pairs of type - ST type names
  -> (CTVarIName, CTypeExpr) -- Pattern variable and its type
  -> CExpr
genRuleFromValSTExpr m iargs insts (y, t) = CApply (genRuleExpr' t) (CVar y)
 where
  genRuleExpr' typ = case typ of
    CTVar i -> case lookup i iargs of
      Just v -> CVar v
      Nothing ->
        error
          $  "genRuleFromValSTExpr: Missing instance argument for variable "
          ++ show i
    CFuncType _ _ ->
      notImplemented "genRuleFromValSTExpr" "Higher-order functions"
    CTCons qname@(_, n1) -> case lookup qname insts of
      Just (_, n2) -> CSymbol $ genQName m n2 n1 "fromValST"
      Nothing ->
        error $ "genRuleFromValSTExpr: Missing instance for " ++ show qname
    CTApply (CTCons ("ST", "ST")) x -> genRuleExpr' x
    CTApply f                     x -> CApply (genRuleExpr' f) (genRuleExpr' x)


genRuleToST :: Monad m => Int -> QName -> OptState m CRule
genRuleToST tvars qn = do
  vs <- freshVars tvars
  let rule   = CRule (map CPVar vs) rhs
      uneval = CApply (CSymbol ("Prelude", ".")) (CSymbol ("ST", "Uneval"))
      exp    = CApply uneval (listToExpr qn (map CVar vs))
      rhs    = CSimpleRhs exp []
  return rule

genRuleFromST :: Monad m => Int -> QName -> OptState m CRule
genRuleFromST tvars qn = do
  vs <- freshVars tvars
  let rule = CRule (map CPVar vs) rhs
      mape = listToExpr ("Prelude", "map") [listToExpr qn (map CVar vs)]
      exp  = listToExpr ("Prelude", ".") [mape, (CSymbol ("ST", "stValues"))]
      rhs  = CSimpleRhs exp []
  return rule

