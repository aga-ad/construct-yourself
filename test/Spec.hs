{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term(..), bound, free, fresh, substitute, reduce, alpha, beta, eta, equals, notEquals)
import           Test.Hspec
import Data.Set


main :: IO ()
main = hspec $ do
    describe "Fresh test" testFresh
    describe "Substitution test" $ do
        testSubstitution1
        testSubstitution2
    describe "Full test" $ do
        test1
        test2
        test3

testFresh :: SpecWith ()
testFresh = do
    let conflicts = fromList ["a", "x", "x1"]
    it "should generate fresh names" $
        toList conflicts `shouldNotContain` [fresh conflicts]

testSubstitution1 :: SpecWith ()
testSubstitution1 = do
    let t = Lam "x" $ Var "x"
    it "substitution 1" $
        t == (substitute t "a" $ App t t)

testSubstitution2 :: SpecWith ()
testSubstitution2 = do
    let boundedA = Lam "a" $ Var "a"
    let boundedA' = substitute boundedA "a" $ Var "b"
    let boundedB = Lam "b" $ Var "b"
    let t = App boundedA boundedB
    let a = App t $ App t $ App (Var "a") $ Var "c"
    let b = App t $ App t $ App (Var "b") $ Var "c"
    let a0 = substitute a "b" $ Var "a"
    let b0 = substitute a "a" $ Var "b"
    it "substitution 2" $
        boundedA `equals` boundedA'
     && (reduce b0) `equals` (reduce b)
     && a0 `equals` a
     && (substitute a0 "c" b0) `equals` (substitute a "c" b)


test1 :: SpecWith ()
test1 = do
    let boundedA = Lam "a" $ Var "a"
    let boundedB = Lam "b" $ Var "b"
    it "a.a == b.b" $
        boundedA `equals` boundedB

test2 :: SpecWith ()
test2 = do
    let a = Lam "a" $ Var "a"
    let aa = App a a
    let aaa = App a aa
    let aaaa = App a aaa
    it "(a.a) ((a.a) ((a.a) (a.a))) == a.a" $
        aaaa `equals` a

test3 :: SpecWith ()
test3 = do
    let a = Lam "a" $ Lam "b" $ Var "c"
    let b = Lam "d" $ Lam "e" $ Var "c"
    let c = Lam "d" $ Lam "e" $ Var "f"
    it "a.b.c == d.e.c and a.b.c != d.e.f" $
        a `equals` b
     && a `notEquals` c
