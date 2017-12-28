{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Construction.Internal.TypeFunctions where

import qualified Data.Map                        as M ((!), map, member, union, fromList, keys, singleton)
import           Data.Text                       (pack)
import           Data.Map                        (member, fromList)
import           Data.Set                        (Set (..), elemAt, delete, singleton, toList)
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
              in Substitution $ M.fromList $ map (\tp -> (tp, substitute t $ substitute s $ TVar tp)) keytypes

-- Create new context from free variables of some term
contextFromTerm :: Term -> Context
contextFromTerm term = Context $ fromList $ zip (toList $ free term) vars
  where
    vars = fmap (TVar . pack . ('a':) . show) [1..]

-- Find a substitution that can solve the set of equations
u :: Set Equation -> Maybe Substitution
u set | null set  = pure mempty
      | otherwise = let (x, rest) = split set
                    in undefined

u1 :: Type -> Type -> Maybe Substitution
u1 a@(TVar n) b | a == b = Just mempty
                | n `contained` b = Nothing
                | otherwise = Just $ Substitution $ M.singleton n b
u1 a b@(TVar _) = u1 b a
u1 (TArr a1 a2) (TArr b1 b2) = do
                                s <- u1 a2 b2
                                res <- u1 (substitute s a1) (substitute s b1)
                                return (compose s res)


contained :: Name -> Type -> Bool
n `contained` (TVar a) = n == a
n `contained` (TArr a b) = n `contained` a && n `contained` b


-- Generate equations set from some term
-- NB: you can use @fresh@ function to generate type names
e :: Context -> Term -> Type -> Maybe (Set Equation)
e ctx term tpe = case term of
                   Var{..} -> undefined--(\x -> singleton (tpe,x)) <$> ctx ! var
                   App{..} -> undefined
                   Lam{..} -> undefined

-- Find a principal pair of some term if exists
pp :: Term -> Maybe (Context, Type)
pp term = do let ctx = contextFromTerm term
             let tpe = TVar "r"
             eqs <- e ctx term tpe
             subs <- u eqs
             pure (substitute subs ctx, substitute subs tpe)
