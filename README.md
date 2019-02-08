Implementation of 'Synthesizing Set Functions'
===============================================

A prototype implementation of generating set functions as presented
in [Synthesizing Set Functions](https://arxiv.org/abs/1808.07401).

## Installation
Execute `cypm install` (due to a bug in KiCS2, only PAKCS is currently supported).
This installs the executable `synsetfun` in the bin directory of CPM.

## Usage
For an existing Curry Module Examples.curry
```haskell
module Examples where

anyOf :: [Int] -> Int
anyOf (x:xs) = x ? anyOf xs
```
the command
```shell
synsetfun Examples -f anyOf
```
generates the output 
```haskell

instance NF t0 => NF (STList t0) where
  nf v9 =
    case v9 of
      STNil -> Val STNil
      STCons v10 v11 ->
        case nfST v10 of
          Choice v12 v13 v14 -> Choice v12 (nf (STCons v13 v11)) (nf (STCons v14 v11))
          Fail -> Fail
          v15 ->
            case nfST v11 of
              Choice v16 v17 v18 -> Choice v16 (nf (STCons v15 v17)) (nf (STCons v15 v18))
              Fail -> Fail
              v19 -> Val (STCons v15 v19)

data STList t0 = STNil | STCons (ST t0) (ST (STList t0))

choiceP :: IDSupply -> ST t0 -> ST t0 -> ST t0
choiceP v2 v_1 v_2 = Choice v1 v_1 v_2 where v1 = uniqueID v2

anyOfP :: IDSupply -> ST (STList Int) -> ST Int
anyOfP v6 v_1 = applyST
  (\v0 -> case v0 of
    STCons v_2 v_3 -> choiceP v5 v_2 (anyOfP v4 v_3)
    STNil          -> Fail
  )
  v_1
 where
  v5 = rightSupply v6
  v7 = leftSupply v6
  v4 = rightSupply v7

toValST_List_STList :: (t0 -> ST t1) -> [t0] -> STList t1
toValST_List_STList v20 [] = STNil
toValST_List_STList v21 (v22 : v23) =
  STCons (v21 v22) (toST_List_STList v21 v23)

fromValST_List_STList :: (t0 -> t1) -> STList t0 -> [t1]
fromValST_List_STList v24 STNil = []
fromValST_List_STList v25 (STCons (Val v26) (Val v27)) =
  v25 v26 : fromValST_List_STList v25 v27

toST_List_STList :: (t0 -> ST t1) -> [t0] -> ST (STList t1)
toST_List_STList v28 = Uneval . toValST_List_STList v28

fromST_List_STList :: NF t0 => (t0 -> t1) -> ST (STList t0) -> Values [t1]
fromST_List_STList v29 = map (fromValST_List_STList v29) . stValues

toValST_Int_Int :: Int -> Int
toValST_Int_Int = id

fromValST_Int_Int :: Int -> Int
fromValST_Int_Int = id

toST_Int_Int :: Int -> ST Int
toST_Int_Int = Uneval . toValST_Int_Int

fromST_Int_Int :: ST Int -> Values Int
fromST_Int_Int = map fromValST_Int_Int . stValues

anyOfS :: [Int] -> Values Int
anyOfS v30 =
  fromST_Int_Int (anyOfP initSupply (toST_List_STList toST_Int_Int v30))
```
Adding the output to the original file and importing the `ST` library should
yield the synthesized set function.

## Limitations
Not supported (yet):
* ~~Sharing and~~ nested set functions
* Polymorphic set functions
* Higher-order functions
* Type synonyms
* External functions
* Record types
* Everything else that doesn't work 🙂
