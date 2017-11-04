{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here,
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta, equals, notEquals
  )where

import           Construction.Internal.Types (Name, Term (..))
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)
import           Data.Text                   (pack)


-- Context is just set of names that are in our context.
type Context = Set Name

-- | @fresh@ generates new variable different from every variables in set.
fresh :: Set Name -> Name
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen -- This is ugly name generator. Make it better.
  where nameGen = [pack $ 'x' : show ind | ind <- [0..] :: [Int]]

-- | @free@ finds all free (Amazing!) variables from given term.
free :: Term -> Set Name
free (Var var)           = singleton var
free (App algo arg)      = free algo `union` free arg
free (Lam variable body) = variable `delete` free body

-- | @bound@ finds all bounded variables from given term.
-- This function uses RecordWildCards.
-- If you like it refactor @free@ function.
bound :: Term -> Set Name
bound Var{}   = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` bound body

-- a[n := b] - substitution
substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n b | var == n = b
                          | otherwise = v
substitute (App algo arg) n b = App (substitute algo n b) $ substitute arg n b
substitute l@(Lam variable body) n b | variable == n = l
                                     | variable `member` frees = substitute (alpha l frees) n b
                                     | otherwise = Lam variable (substitute body n b) where
                                         frees = free b

-- | alpha reduction
alpha :: Term -> Set Name -> Term
alpha v@Var{..} s = v
alpha (App algo arg) s = App (alpha algo s) $ alpha arg s
alpha l@(Lam variable body) s | variable `notMember` s = Lam variable $ alpha body $ variable `insert` s
                              | otherwise = let newVar = fresh $ s `union` (free l)
                                                newBody = substitute body variable $ Var newVar
                                                in Lam newVar $ alpha newBody s



-- | beta reduction
beta :: Term -> Term
beta (App (Lam variable body) arg) = substitute (beta body) variable (beta arg)
beta (App algo arg) = App (beta algo) (beta arg)
beta (Lam variable body) = Lam variable $ beta body
beta v = v

-- | eta reduction
eta :: Term -> Term
eta l@(Lam var1 (App m (Var var2))) | var1 == var2 && var1 `notMember` (free m) = m
                                    | otherwise = l
eta t = t



-- stupidEq :: Term -> Term -> Bool
-- stupidEq (Lam v1 b1) (Lam v2 b2) = (v1 == v2) && (b1 `stupidEq` b2)
-- stupidEq (Var v1) (Var v2) = v1 == v2
-- stupidEq (App algo1 arg1) (App algo2 arg2) = (algo1 `stupidEq` algo2) && (arg1 `stupidEq` arg2)


-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then eta term
                 else reduce term'






instance Eq Term where
-- (==) :: Term -> Term -> Bool
  (==) (Lam v1 b1) (Lam v2 b2) = (substitute b1 v1 freshVariable) == (substitute b2 v2 freshVariable) where
                                  freshVariable = Var $ fresh (free b1 `union` free b2)
  (==) (Var v1) (Var v2) = v1 == v2
  (==) (App algo1 arg1) (App algo2 arg2) = (algo1 == algo2) && (arg1 == arg2)
  (==) _ _ = False






-- Be careful with it. No shit like (\x.x x) (\x.x x)
equals :: Term -> Term -> Bool
equals t1 t2 = equals' (reduce t1) (reduce t2)

equals' (Var v1) (Var v2) | v1 == v2 = True
                          | otherwise = False
equals' (App a1 b1) (App a2 b2) = a1 `equals` a2 && b1 `equals` b2
equals' l1@(Lam var1 body1) l2@(Lam var2 body2) = (substitute body1 var1 newVar) `equals` (substitute body2 var2 newVar) where
                                                    newVar = Var $ fresh ((free l1) `union` (free l2))
equals' _ _ = False

notEquals :: Term -> Term -> Bool
notEquals t1 t2 = not $ equals t1 t2
