--- This module defines the structure and some generic operations
--- for search trees

module ST where

import Control.Findall  ( isFail )

-----------------------------------------------------------------------------
--- Type to represent choice identifiers.
type ID = Int
type IDSupply = Int

initSupply :: IDSupply
initSupply = 1

uniqueID :: IDSupply -> ID
uniqueID i = i

leftSupply :: IDSupply -> IDSupply
leftSupply i = 2 * i

rightSupply :: IDSupply -> IDSupply
rightSupply i = 2 * i + 1

-----------------------------------------------------------------------------
--- Type to represent choice decisions.
data Decision = Left | Right

type DM = [(ID, Decision)]

-----------------------------------------------------------------------------
--- Data type to represent search trees where failures
--- can be inside or outside.
--- Note that the actual values are head normal forms.
--- The evaluation to values will be done when a set functions
--- returns a value set.
data ST a = Val a                   -- a value (in head normal form)
          | Uneval a                -- an unevaluated value from outside
          | Fail                    -- failure
          | Fail0                   -- top-level (outside) failure
          | Choice ID (ST a) (ST a) -- non-deterministic choice with identifier

-- Generic operation to apply a non-deterministic operation defined
-- on a single head normal form to a search tree.
applyST :: (a -> ST b) -> ST a -> ST b
applyST f (Val    x)       = f x
applyST f (Uneval x)       = if isFail x then Fail0 else f x
applyST _ Fail             = Fail
applyST _ Fail0            = Fail0
applyST f (Choice i x1 x2) = Choice i (f `applyST` x1) (f `applyST` x2)

--- A type with the `NF` property must provide a method `nf`
--- to evaluate an expression in head normal form into a search tree
--- where all `Val` arguments are fully evaluated, i.e., do not
--- contain choices or failures in subterms.
class NF a where
  nf :: a -> ST a

--- This operation extends the operation `nf` on search trees.
nfST :: NF a => ST a -> ST a
nfST (Val    x)       = nf x
nfST (Uneval x)       = if isFail x then Fail0 else x `seq` nf x
nfST Fail             = Fail
nfST Fail0            = Fail0
nfST (Choice i x1 x2) = Choice i (nfST x1) (nfST x2)

--- Some NF instances for base types.
instance NF Int where
  nf x = Val x

instance NF Bool where
  nf x = Val x

instance NF Char where
  nf x = Val x

-----------------------------------------------------------------------------
--- Representation of non-deterministic list structures.

-- The head normal form of a list structure with non-deterministic components.
data STList a = Nil | Cons (ST a) (ST (STList a))

instance NF a => NF (STList a) where
  nf Nil = Val Nil
  nf (Cons x xs) = case nfST x of
    Choice n c1 c2 -> Choice n (nfST (Val (Cons c1 xs)))
                               (nfST (Val (Cons c2 xs)))
    Fail -> Fail
    Fail0 -> Fail0
    y -> case nfST xs of
           Choice n c1 c2 -> Choice n (nfST (Val (Cons y c1)))
                                      (nfST (Val (Cons y c2)))
           Fail           -> Fail
           Fail0          -> Fail0
           ys             -> Val (Cons y ys)

-----------------------------------------------------------------------------
--- A type with the `NF` property must provide a method `nf`
--- to evaluate an expression in head normal form into an expression
--- which is fully evaluated.

--- Computes the list of all values (normal forms) represented in a search tree.
stValues :: NF a => ST a -> [a]
stValues = searchDFS . nfST

searchDFS :: ST a -> [a]
searchDFS st = maybe failed id (searchDFS' [] st)
 where
  searchDFS' _ (Val x)          = Just [x]
  searchDFS' _ Fail             = Just []
  searchDFS' _ Fail0            = Nothing
  searchDFS' m (Choice i x1 x2) = case lookup i m of
    Nothing ->
      concVals (searchDFS' ((i, Left) : m) x1) (searchDFS' ((i, Right) : m) x2)
    Just Left  -> searchDFS' m x1
    Just Right -> searchDFS' m x2

  concVals Nothing   ys = ys
  concVals (Just xs) ys = Just (xs ++ maybe [] id ys)

-----------------------------------------------------------------------------
-- The following multiparameter type class is what we would wish for in
-- order to realize an elegant automatic transformation of programs.
-- 
-- class ConvertST a b where
--   toValST :: a -> b
--   fromValST :: b -> a
--
-- The method `toValST` converts a Curry value to its ST representation.
-- 'fromValST` converts a value in ST representation back to
-- its corresponding Curry value. When converting a Curry value to its
-- ST representation, we wrap constructor arguments in a `Val`
-- constructor, which is done by using the `toST` function introduced
-- later. This way we can ensure that we do not evaluate Curry expressions
-- that are not demanded.
-- We would have to provide instances of the multiparameter type class for
-- each data type and its translation, e.g., the following instances.
--
-- instance ConvertST Int Int
-- instance ConvertST a b => ConvertST [a] (STList b)
--
-- However, since Curry currently does not support multiparameter type
-- classes we have to provide specialized implementations of its methods.
--
-- In the following we provide these specialized implementations for the
-- aforementioned instances.

toValST_Int_Int :: Int -> Int
toValST_Int_Int = id

fromValST_Int_Int :: Int -> Int
fromValST_Int_Int = id

toValST_Bool_Bool :: Bool -> Bool
toValST_Bool_Bool = id

fromValST_Bool_Bool :: Bool -> Bool
fromValST_Bool_Bool = id

toValST_List_STList :: (a -> ST b) -> [a] -> STList b
toValST_List_STList _  []       = Nil
toValST_List_STList ta (x : xs) = Cons (ta x) (toST_List_STList ta xs)

fromValST_List_STList :: (a -> b) -> STList a -> [b]
fromValST_List_STList _ Nil = []
fromValST_List_STList fa (Cons (Val x) (Val xs)) =
  fa x : fromValST_List_STList fa xs

------------------------------------------------------------------------------
-- We further need a function that returns a search tree representation of
-- a Curry value by wrapping the value translated into its ST representation
-- in a `Val` constructor. Aside from the use in `toValST` we will need
-- this functionality when calling a set function where we wrap every arguments
-- this way.
--
-- toST :: ConvertST a b => a -> ST b
-- toST = Uneval . toValST
--
-- Because we do not have the multiparameter type class `HNF` available
-- in Curry, we have to provide specialized implementations of this
-- overloaded function.

toST_Int_Int :: Int -> ST Int
toST_Int_Int = Uneval . toValST_Int_Int

toST_Bool_Bool :: Bool -> ST Bool
toST_Bool_Bool = Uneval . toValST_Bool_Bool

toST_List_STList :: (a -> ST b) -> [a] -> ST (STList b)
toST_List_STList ta = Uneval . toValST_List_STList ta

------------------------------------------------------------------------------
-- For simplicity, we represent multisets of values as lists:
type Values a = [a]

-- Also, we need a function that translates a search tree representation
-- to the multiset of its Curry values.
--
-- fromST :: (ConvertST a b, NF b) => ST b -> Values a
-- fromST = map fromValST . stValues

-- Like before, we have to provide specialized implementations of this
-- overloaded function.

fromST_Int_Int :: ST Int -> Values Int
fromST_Int_Int = map fromValST_Int_Int . stValues

fromST_Bool_Bool :: ST Bool -> Values Bool
fromST_Bool_Bool = map fromValST_Bool_Bool . stValues

fromST_STList_List :: NF a => (a -> b) -> ST (STList a) -> Values [b]
fromST_STList_List fa = map (fromValST_List_STList fa) . stValues

------------------------------------------------------------------------------
-- Lifting functions to plural functions:

-- Lift a unary function into a plural function:
lift1P :: (a -> ST b) -> ST a -> ST b
lift1P f xp = f `applyST` xp

-- Lift a binary function into a plural function:
lift2P :: (a -> b -> ST c) -> ST a -> ST b -> ST c
lift2P f xp yp = lift2X `applyST` xp
  where lift2X x = lift2XY `applyST` yp where lift2XY y = f x y

-- Lift a ternary function into a plural function:
lift3P :: (a -> b -> c -> ST d) -> ST a -> ST b -> ST c -> ST d
lift3P f xp yp zp = lift3X `applyST` xp
 where
  lift3X x = lift3XY `applyST` yp
    where lift3XY y = lift3XYZ `applyST` zp where lift3XYZ z = (f x y z)

------------------------------------------------------------------------------
