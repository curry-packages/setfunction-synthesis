-- Module that adds missing branches of partial case expressions
-- in a FlatCurry program
module Totalize
  ( totalizeProg
  )
where

import           FlatCurry.Goodies
import           FlatCurry.Types

totalizeProg :: Prog -> Prog
totalizeProg (Prog name imports tydecls funcdecls opdecls) =
  let consTypePs tydecl = case tydecl of
        Type    _ _ _ cds -> map (flip (,) tydecl . consName) cds
        TypeSyn _ _ _ _   -> [] -- todo           
      consTypeMap = concatMap consTypePs tydecls
      funcdecls'  = map (totalizeFuncDecl consTypeMap) funcdecls
  in  Prog name ("Prelude" : imports) tydecls funcdecls' opdecls

totalizeFuncDecl :: [(QName, TypeDecl)] -> FuncDecl -> FuncDecl
totalizeFuncDecl consTypeMap (Func qn ar vis ty r) =
  Func qn ar vis ty (totalizeRule consTypeMap r)

totalizeRule :: [(QName, TypeDecl)] -> Rule -> Rule
totalizeRule _           (External s) = External s
totalizeRule consTypeMap (Rule vs e ) = Rule vs (totalizeExpr consTypeMap e)

totalizeExpr :: [(QName, TypeDecl)] -> Expr -> Expr
totalizeExpr consTypeMap expr
  = let te = totalizeExpr consTypeMap
    in
      case expr of
        Var _              -> expr
        Lit _              -> expr
        Comb ct qn es      -> Comb ct qn (map te es)
        Let   bs exp       -> Let (map (\(v, e) -> (v, te e)) bs) exp
        Free  vs e         -> Free vs (te e)
        Or    e1 e2        -> Or (te e1) (te e2)
        Typed e  ty        -> Typed (te e) ty
        Case _  _ []       -> expr
        Case ct e (b : bs) -> Case ct e (bs' ++ missingBranches)
         where
          Just typ = lookup (patCons $ branchPattern b) consTypeMap
          consQNs  = map (\(FlatCurry.Types.Cons qn ar _ _) -> (qn, ar))
                         (typeConsDecls typ)
          branchQNs  = map (patCons . branchPattern) (b : bs)
          missingQNs = filter (\(qn, _) -> qn `notElem` branchQNs) consQNs
          failed     = Comb FuncCall ("Prelude", "failed") []
          newPat qn ar = Pattern qn [1000 .. 1000 + ar - 1] -- todo
          newBranch (qn, ar) = Branch (newPat qn ar) failed
          missingBranches = map newBranch missingQNs
          bs'             = map (\(Branch p exp) -> Branch p (te exp)) (b : bs)

