{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Construction.Internal.TypeFunctions where

import qualified Data.Map                        as M ((!), map, member, union, fromList, keys, singleton, elems)
import           Data.Text                       (pack)
import           Data.Map                        (member, fromList)
import           Data.Set                        (Set (..), elemAt, delete, singleton, toList, union, insert, map)
import           Construction.Internal.Types
import           Construction.Internal.Functions hiding (Context, substitute)

-- Split a set of elements to the first element and rest set
split :: Ord a => Set a -> (a, Set a)
split set = let x = elemAt 0 set
            in  (x, delete x set)

-- Take variable type from context or return Nothing
(!) :: Context -> Name -> Maybe Type
ctx ! x | member x (getCtx ctx) = Just $ getCtx ctx M.! x
        | otherwise             = Nothing

-- Something we can perform substitution with
class Substitutable a where
  substitute :: Substitution -> a -> a

-- Substitution in context
--   [a:=t]empty       => empty
--   [a:=t]{x:t1 ... } => {x:([a:=t]t1) ... }
instance Substitutable Context where
  substitute sbst ctx = Context $ M.map (substitute sbst) (getCtx ctx)

-- Substitution in type:
--   [a:=t] a     => t
--   [a:=t] b     => b
--   [a:=t](r->p) => ([a:=t]r)->([a:=t]p)
instance Substitutable Type where
  substitute sbst t@(TVar name) | M.member name (getSubs sbst) = (getSubs sbst) M.! name
                                | otherwise = t
  substitute sbst (TArr from to) = TArr (substitute sbst from) (substitute sbst to)


-- Compose two substitutions
-- S@[a1 := t1, ...] . [b1 := s1 ...] = [b1 := S(s1) ... a1 := t1 ...] ???????????????????????????????????????????
-- T◦S
compose :: Substitution -> Substitution -> Substitution
compose s t = let keytypes = M.keys $ M.union (getSubs s) (getSubs t)
              in Substitution $ M.fromList $ Prelude.map (\tp -> (tp, substitute t $ substitute s $ TVar tp)) keytypes

-- Create new context from free variables of some term
contextFromTerm :: Term -> Context
contextFromTerm term = Context $ fromList $ zip (toList $ free term) vars
  where
    vars = fmap (TVar . pack . ('a':) . show) [1..]

-- Find a substitution that can solve the set of equations
u :: Set Equation -> Maybe Substitution
-- u set | null set  = pure mempty
--       | otherwise = do let ((a, b), rest) = split set
--                        s1 <- u1 a b
--                        s2 <- u rest
--                        return (compose s2 s1)
u set | null set  = pure mempty
      | otherwise = let (e, xs) = split set
                    in case e of
                      (a@(TVar n), b) -> uhelper a b where
                                           uhelper a@(TVar n) b | a == b = u xs
                                                                | n `contained` b = Nothing
                                                                | otherwise = do let sub = Substitution $ M.singleton n b
                                                                                 subs <- u $ Data.Set.map (\(t1, t2) -> (substitute sub t1, substitute sub t2)) xs
                                                                                 return (compose sub subs)
                      (b, a@(TVar n)) -> u $ (a, b) `insert` xs
                      ((TArr a1 a2), (TArr b1 b2)) -> u $ (a1, b1) `insert` ((a2, b2) `insert` xs)

u1 :: Type -> Type -> Maybe Substitution
u1 t1 t2 = u $ singleton (t1, t2)
-- u1 a@(TVar n) b | a == b = Just mempty
--                 | n `contained` b = Nothing
--                 | otherwise = Just $ Substitution $ M.singleton n b
-- u1 a b@(TVar _) = u1 b a
-- u1 (TArr a1 a2) (TArr b1 b2) = do s <- u1 a2 b2
--                                   res <- u1 (substitute s a1) (substitute s b1)
--                                   return (compose s res)


contained :: Name -> Type -> Bool
n `contained` (TVar a) = n == a
n `contained` (TArr a b) = n `contained` a || n `contained` b


-- Generate equations set from some term
-- NB: you can use @fresh@ function to generate type names
e :: Context -> Term -> Type -> Maybe (Set Equation)
e ctx term tpe = e' ctx term tpe ((typeNames tpe) `union` (typeNamesL $ M.elems (getCtx ctx)))
e' ctx term tpe booked = case term of
                   Var{..} -> (\x -> singleton (tpe, x)) <$> ctx ! var
                   App m n -> do let new = TVar $ fresh booked
                                 let booked1 = (tvar new) `insert` booked
                                 e1 <- e' ctx m (TArr new tpe) booked1
                                 let booked2 = booked1 `union` typeNamesSE e1
                                 e2 <- e' ctx n new booked2
                                 return (e1 `mappend` e2)
                   Lam var body -> do let new1 = TVar $ fresh booked
                                      let booked1 = (tvar new1) `insert` booked
                                      let new2 = TVar $ fresh booked1
                                      let booked2 = (tvar new2) `insert` booked1
                                      e1 <- e' (ctx `mappend` (Context $ M.singleton var new1)) body new2 booked2
                                      return ((tpe, TArr new1 new2) `insert` e1)

typeNames :: Type -> Set Name
typeNames (TVar n) = singleton n
typeNames (TArr a b) = typeNames a `union` typeNames b

typeNamesL :: [Type] -> Set Name
typeNamesL [] = mempty
typeNamesL (x:xs) = typeNames x `union` typeNamesL xs

typeNamesSE :: Set Equation -> Set Name
typeNamesSE s | null s = mempty
              | otherwise = let ((t1, t2), xs) = split s
                            in (typeNames t1) `union` (typeNames t2) `union` typeNamesSE xs

-- Find a principal pair of some term if exists
pp :: Term -> Maybe (Context, Type)
pp term = do let ctx = contextFromTerm term
             let tpe = TVar "r"
             eqs <- e ctx term tpe
             subs <- u eqs
             pure (substitute subs ctx, substitute subs tpe)
