-- This module provides functions to look up type and functions in
-- both FlatCurry and AbstractCurry programs.
module Lookup where

import           AbstractCurry.Types     hiding ( QName )
import           FlatCurry.Types

import           State
import           StateMonad

lookupTypeDecl :: Monad m => QName -> OptState m TypeDecl
lookupTypeDecl qn = do
  (Prog _ _ tds _ _) <- gets currentProg
  lookupTypeDecl' tds
 where
  lookupTypeDecl' [] =
    error $ "lookupTypeDecl: Missing type declaration for " ++ show qn
  lookupTypeDecl' (td : tds) = case td of
    (Type    tqn _ _ _) -> if qn == tqn then return td else lookupTypeDecl' tds
    (TypeSyn _   _ _ _) -> lookupTypeDecl' tds

lookupFuncDecl :: Monad m => QName -> OptState m FuncDecl
lookupFuncDecl qn = do
  (Prog _ _ _ fds _) <- gets currentProg
  lookupFuncDecl' fds
 where
  lookupFuncDecl' [] =
    error $ "lookupFuncDecl: Missing function declaration for " ++ show qn
  lookupFuncDecl' (fd : fds) =
    let (Func fqn _ _ _ _) = fd
    in  if qn == fqn then return fd else lookupFuncDecl' fds

lookupCFuncDecl :: Monad m => QName -> OptState m CFuncDecl
lookupCFuncDecl qn = do
  (CurryProg _ _ _ _ _ _ fds _) <- gets currentCProg
  lookupCFuncDecl' fds
 where
  lookupCFuncDecl' [] =
    error $ "lookupCFuncDecl: Missing function declaration for " ++ show qn
  lookupCFuncDecl' (fd : fds) =
    let (CFunc fqn _ _ _ _) = fd
    in  if qn == fqn then return fd else lookupCFuncDecl' fds

-- Whenever both an original FlatCurry type or a generated AbstractCurry type
-- could occur, this function is necessary.
lookupCTypeDecl :: Monad m => QName -> OptState m (Either TypeDecl CTypeDecl)
lookupCTypeDecl qn = do
  (CurryProg _ _ _ _ _ tds _ _) <- gets currentCProg
  lookupCTypeDecl' tds
 where
  lookupCTypeDecl' []         = lookupTypeDecl qn >>= (return . Prelude.Left)
  lookupCTypeDecl' (td : tds) = case td of
    (CType tqn _ _ _ _) ->
      if qn == tqn then return $ Prelude.Right td else lookupCTypeDecl' tds
    _ -> lookupCTypeDecl' tds

