-- This module generates set functions.
module GenSetFunction
  ( genSetFunction
  )
where

import           AbstractCurry.Types     hiding ( QName )
import           FlatCurry.Types
import           List                           ( nub )

import           State
import           StateMonad
import           Utilities
import           Lookup
import           Translate

-- Generates a set function for the current function
genSetFunction :: Monad m => OptState m ()
genSetFunction = do
  m                   <- gets currentModule
  qn                  <- gets currentFunction
  (Func _ ar vis t _) <- lookupFuncDecl qn
  vs                  <- freshVars ar
  ftys                <- gets funcTypes
  let insts      = nub $ concatMap compareTypes ftys
      instsST    = map swap insts
      ct         = translTypeExpr t
      (argts, _) = typeList t
      sftyp      = CQualType (CContext []) (setFunctionType ct)
      qnS        = addQNPostfix "S" qn
      qnP        = addQNPostfix "P" qn
      vis'       = translVis vis
      exprs      = map (genSetExprToST m insts) argts
  (CFunc _ _ _ (CQualType _ pt) _) <- lookupCFuncDecl qnP
  let (_, retTp) = ctypeList pt
      expr       = genSetExprFromST m instsST retTp
      exprs'     = map (\(e, v) -> CApply e (CVar v)) (zip exprs vs)
      exprs''    = CSymbol ("ST", "initSupply") : exprs'
      rhs        = CSimpleRhs (CApply expr (listToExpr qnP exprs'')) []
      rule       = CRule (map CPVar vs) rhs
      fd         = CFunc qnS ar vis' sftyp [rule]
  modify $ addCurryFDs [fd]

-- Generates the toST part of the set function's expression
genSetExprToST :: String -> [(QName, QName)] -> TypeExpr -> CExpr
genSetExprToST m insts typ
  = let genS = genSetExprToST m insts
    in
      case typ of
        TVar _ -> notImplemented "genSetExprToST" "Polymorphic functions"
        FuncType _ _ ->
          notImplemented "genSetExprToST" "Higher-order functions"
        TCons qname@(_, n1) ts -> case lookup qname insts of
          Just (_, n2) ->
            let args   = map genS ts
                qname' = genQName m n1 n2 "toST"
            in  listToExpr qname' args
          Nothing ->
            error $ "genSetExprToST: Missing instance for " ++ show qname
        ForallType _ ty -> genS ty

-- Generates the fromST part of the set function's expression
genSetExprFromST :: String -> [(QName, QName)] -> CTypeExpr -> CExpr
genSetExprFromST m insts typ
  = let genS = genSetExprFromST m insts
    in
      case typ of
        CTVar _ -> notImplemented "genSetExprFromST" "Polymorphic functions"
        CFuncType _ _ ->
          notImplemented "genSetExprFromSt" "Higher-order functions"
        CTCons qname@(_, n1) -> case lookup qname insts of
          Just (_, n2) -> CSymbol $ genQName m n2 n1 "fromST"
          Nothing ->
            error $ "genSetExprFromSt: Missing instance for " ++ show qname
        CTApply (CTCons ("ST", "ST")) x -> genS x
        CTApply f                     x -> CApply (genS f) (genS x)

-- Adds a Values constructor to the return type of a function type
setFunctionType :: CTypeExpr -> CTypeExpr
setFunctionType t = case t of
  CFuncType d r -> CFuncType d (setFunctionType r)
  _             -> addValues t
