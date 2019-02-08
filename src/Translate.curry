-- This module translates FlatCurry to AbstractCurry
module Translate where

import           AbstractCurry.Types     hiding ( QName
                                                , CVisibility(..)
                                                )
import qualified AbstractCurry.Types           as ACT
                                                ( CVisibility(..) )
import           FlatCurry.Annotated.Types
import           FlatCurry.Types

import           Utilities

-- AbstractCurry translation
----------------------------
translFuncDecl :: AFuncDecl TypeExpr -> CFuncDecl
translFuncDecl (AFunc qn ar vis ty r) = CFunc
  qn
  ar
  (translVis vis)
  (CQualType (CContext []) (translTypeExpr ty))
  [translRule r]

translRule :: ARule TypeExpr -> CRule
translRule (ARule _ vs e) = CRule
  (map (\(v, _) -> CPVar (translExistingVar v)) vs)
  (CSimpleRhs (translExpr e) [])
translRule (AExternal _ _) = notImplemented "translRule" "External rules"

translExpr :: AExpr TypeExpr -> CExpr
translExpr exp = case exp of
  AVar _ i                        -> CVar (translExistingVar i)
  ALit _ l                        -> CLit (translLit l)
  AComb _ _ (qn, _) []            -> CSymbol qn
  AComb _ _ (qn, _) exprs@(_ : _) -> listToExpr qn (map translExpr exprs)
  ALet _ bs expr                  -> CLetDecl ldecls (translExpr expr)
   where
    ldecls = map
      (\((v, _), e) ->
        CLocalPat (CPVar (translExistingVar v)) (CSimpleRhs (translExpr e) [])
      )
      bs
  AFree _ vs e -> CLetDecl [fvars] (translExpr e)
    where fvars = CLocalVars (map translExistingVar (fst $ unzip vs))
  AOr _ e1 e2 ->
    CApply (CApply (CSymbol ("Prelude", "?")) (translExpr e1)) (translExpr e2)
  ACase _ ct e brs -> CCase (translCaseType ct) (translExpr e) branches
    where branches = map translBranch brs
  ATyped _ e ty ->
    CTyped (translExpr e) (CQualType (CContext []) (translTypeExpr ty))

translBranch :: ABranchExpr TypeExpr -> (CPattern, CRhs)
translBranch (ABranch (APattern _ (qn, _) vs) e) =
  ( CPComb qn (map (CPVar . translExistingVar) (map fst vs))
  , CSimpleRhs (translExpr e) []
  )
translBranch (ABranch (ALPattern _ l) e) =
  (CPLit (translLit l), CSimpleRhs (translExpr e) [])

translVis :: Visibility -> ACT.CVisibility
translVis Public  = ACT.Public
translVis Private = ACT.Private

translTVar :: TVarIndex -> CTVarIName
translTVar v = (v, 't' : show v)

translVar :: VarIndex -> CVarIName
translVar v = (v, 'v' : show v)

translExistingVar :: VarIndex -> CVarIName
translExistingVar v = (v, "v_" ++ show v)

translTypeDecl :: TypeDecl -> CTypeDecl
translTypeDecl td =
  let tvar = translTVar
      tvis = translVis
      tcd  = translConsDecl
  in  case td of
        Type qn vis vs cds ->
          CType qn (tvis vis) (map tvar vs) (map tcd cds) []
        TypeSyn qn vis vs ty ->
          CTypeSyn qn (tvis vis) (map tvar vs) (translTypeExpr ty)

translConsDecl :: ConsDecl -> CConsDecl
translConsDecl (FlatCurry.Types.Cons qn _ vis tys) =
  CCons [] {- <- todo -}
           (CContext []) qn (translVis vis) (map translTypeExpr tys)

translTypeExpr :: TypeExpr -> CTypeExpr
translTypeExpr ty = case ty of
  TVar i                    -> CTVar $ translTVar i
  FuncType   d  r           -> CFuncType (translTypeExpr d) (translTypeExpr r)
  TCons      qn []          -> CTCons qn
  TCons      qn tys@(_ : _) -> listToType qn (map translTypeExpr tys)
  ForallType _  typ         -> translTypeExpr typ

translLit :: Literal -> CLiteral
translLit l = case l of
  Intc   i -> CIntc i
  Floatc f -> CFloatc f
  Charc  c -> CCharc c

translCaseType :: CaseType -> CCaseType
translCaseType Flex  = CFlex
translCaseType Rigid = CRigid

