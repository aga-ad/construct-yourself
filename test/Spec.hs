{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term(..), bound, free, fresh, substitute)
import           Test.Hspec
import Data.Set


main :: IO ()
main = hspec $ do
    describe "Fresh test" testFresh
    describe "Substitution test" $ do
        testSubstitution1
        testSubstitution2

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
    let bounedA = Lam "a" $ Var "a"
    let bounedB = Lam "b" $ Var "b"
    let t = App bounedA bounedB
    let a = App t $ App t $ App (Var "a") $ Var "c"
    let b = App t $ App t $ App (Var "b") $ Var "c"
    let a0 = substitute a "b" $ Var "a"
    let b0 = substitute a "a" $ Var "b"
    it "substitution 2" $
        b0 == b &&
        a0 == a &&
        (substitute a0 "c" b0) == (substitute a "c" b)
