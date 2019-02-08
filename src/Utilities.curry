-- This module contains utility functions.
module Utilities where

import           AbstractCurry.Types     hiding ( QName )
import           FlatCurry.Types
import           List                           ( nub )
import           Integer                        ( even
                                                , odd
                                                )

-- AbstractCurry equivalent to FlatCurry BranchExpr
type CBranchExpr = (CPattern, CRhs)

cbranch :: CPattern -> CRhs -> CBranchExpr
cbranch = (,)

-- Applies a type constructor to a list of argument types
listToType :: QName -> [CTypeExpr] -> CTypeExpr
listToType qn exps = listToType' (reverse exps)
 where
  listToType' []               = CTCons qn
  listToType' [e             ] = CTApply (CTCons qn) e
  listToType' (e : es@(_ : _)) = CTApply (listToType' es) e

-- Applies a function or constructor to a list of argument expressions
listToExpr :: QName -> [CExpr] -> CExpr
listToExpr qn exps = listToExpr' (reverse exps)
 where
  listToExpr' []               = CSymbol qn
  listToExpr' [e             ] = CApply (CSymbol qn) e
  listToExpr' (e : es@(_ : _)) = CApply (listToExpr' es) e


notImplemented :: String -> String -> a
notImplemented eWhere eWhat = error $ eWhere ++ ": Not implemented: " ++ eWhat
showQName :: QName -> String
showQName (m, n) = "(" ++ m ++ ", " ++ n ++ ")"

-- A type declaration where every constructor is nullary is considered basic.
isBasicType :: TypeDecl -> Bool
isBasicType (Type _ _ _ cs) = foldr (\c b -> isConstCons c && b) True cs
  where isConstCons (FlatCurry.Types.Cons _ arity _ _) = arity == 0
isBasicType (TypeSyn _ _ _ _) = False -- todo

addQNPrefix :: String -> QName -> QName
addQNPrefix s (m, n) = (m, s ++ n)

addQNPostfix :: String -> QName -> QName
addQNPostfix s (m, n) = (m, n ++ s)

addST :: CTypeExpr -> CTypeExpr
addST ty = CTApply (CTCons ("ST", "ST")) ty

removeST :: CTypeExpr -> CTypeExpr
removeST t = case t of
  CTApply (CTCons ("ST", "ST")) t' -> t'
  CFuncType d r -> CFuncType (removeST d) (removeST r)
  _ -> t

addValues :: CTypeExpr -> CTypeExpr
addValues ty = CTApply (CTCons ("ST", "Values")) ty

renameType, renameCons, renameFunc :: String -> String
renameType n | n == "[]"  = "List"
             | n == "(,)" = "Pair"
             | otherwise  = n
renameFunc n | n == "?"  = "choice"
             | otherwise = n
renameCons n | n == ":"   = "Cons"
             | n == "[]"  = "Nil"
             | n == "(,)" = "PairCons"
             | otherwise  = n

-- Splits a FlatCurry type into a list of argument types and a return type
typeList :: TypeExpr -> ([TypeExpr], TypeExpr)
typeList ty = case ty of
  FuncType _ _ -> typeList' ty
  _            -> ([], ty)
 where
  typeList' typ = case typ of
    FuncType d@(FuncType _ _) r -> let (ts, t) = typeList' r in ([d] ++ ts, t)
    FuncType d r@(FuncType _ _) ->
      let (tsd, _) = typeList' d
          (tsr, t) = typeList' r
      in  (tsd ++ tsr, t)
    FuncType d r -> let (ts, _) = typeList' d in (ts, r)
    _            -> ([typ], typ)

-- Splits an AbstractCurry type into a list of argument types and a return type
ctypeList :: CTypeExpr -> ([CTypeExpr], CTypeExpr)
ctypeList ty = case ty of
  CFuncType _ _ -> ctypeList' ty
  _             -> ([], ty)
 where
  ctypeList' typ = case typ of
    CFuncType d@(CFuncType _ _) r ->
      let (ts, t) = ctypeList' r in ([d] ++ ts, t)
    CFuncType d r@(CFuncType _ _) ->
      let (tsd, _) = ctypeList' d
          (tsr, t) = ctypeList' r
      in  (tsd ++ tsr, t)
    CFuncType d r -> let (ts, _) = ctypeList' d in (ts, r)
    _             -> ([typ], typ)

-- Returns a list of type constructor names that are structurally at the same position
-- within a type expression.
compareTypes :: (CTypeExpr, CTypeExpr) -> [(QName, QName)]
compareTypes (orig, transformed) = compareTypes' (orig, removeST transformed)
 where
  compareTypes' p = case p of
    (CFuncType d1 r1, CFuncType d2 r2) ->
      compareTypes' (d1, d2) ++ compareTypes' (r1, r2)
    (CTApply f1 x1, CTApply f2 x2) ->
      compareTypes' (f1, f2) ++ compareTypes' (x1, x2)
    (CTCons qn1, CTCons qn2) -> [(qn1, qn2)]
    _                        -> []

genQName :: String -> String -> String -> String -> QName
genQName m n1 n2 s = (m, s ++ "_" ++ renameType n1 ++ "_" ++ renameType n2)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

addMod :: String -> [QName] -> [QName]
addMod mod qns = map (\(m, n) -> if m == "" then (mod, n) else (m, n)) qns

evens :: [Int]
evens = filter even [0 ..]

odds :: [Int]
odds = filter odd [1 ..]

typeVars :: TypeExpr -> [TVarIndex]
typeVars = nub . typeVars'
 where
  typeVars' typ = case typ of
    TVar i          -> [i]
    FuncType   d r  -> typeVars' d ++ typeVars' r
    TCons      _ ts -> concatMap typeVars' ts
    ForallType _ t  -> typeVars' t

ctypeVars :: CTypeExpr -> [CTVarIName]
ctypeVars = nub . ctypeVars'
 where
  ctypeVars' t = case t of
    CTVar i       -> [i]
    CFuncType d r -> ctypeVars' d ++ ctypeVars' r
    CTCons _      -> []
    CTApply f x   -> ctypeVars' f ++ ctypeVars' x
