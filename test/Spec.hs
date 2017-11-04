{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Construction (Name, Term (..), bound, free, fresh, alpha, beta, eta, reduce, substitute, equals, notEquals)
import           Test.Hspec
import Data.Set


main :: IO ()
main = hspec $ do
    describe "My tests" $ do
        testSubstitution1
        testSubstitution2
        test1
        test2
        test3
    describe "Fresh test" testFresh
    describe "Free test" testFree
    describe "Bound test" testBound
    describe "Alpha test" testAlpha
    describe "Substitution test" testSubs
    describe "Beta test" testBeta
    describe "Eta test" testEta
    describe "Reduce test" testReduce
    describe "Equal test" testEqual


varX, varX0, varX1, varX2 :: Term
varX = Var "x"
varX0 = Var "x0"
varX1 = Var "x1"
varX2 = Var "x2"

lamId, lamK, lamKK :: Term
lamId = Lam "x" varX -- \x.x
lamK  = Lam "x" $ Lam "x1" varX -- \x x1.x == \x.(\x1.x)
lamKK = Lam "x" $ Lam "x1" varX1 -- \x x1.x1 == \x.(\x1.x1)

appXX, appXX1 :: Term
appXX = App varX varX -- (x x)
appXX1 = App varX varX1 -- (x x1)


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



testFree :: SpecWith ()
testFree = do
    it "#1" $ free varX `shouldBe` singleton "x"
    it "#2" $ free varX1 `shouldBe` singleton "x1"
    it "#3" $ free lamId `shouldBe` empty
    it "#4" $ free lamK `shouldBe` empty
    it "#5" $ free lamKK `shouldBe` empty
    it "#6" $ free appXX  `shouldBe` singleton "x"
    it "#7" $ free (Lam "x" appXX) `shouldBe` empty
    it "#8" $ free (Lam "x" appXX1) `shouldBe` singleton "x1"
    it "#9" $ free (Lam "x2" appXX1) `shouldBe` fromList ["x", "x1"]

testBound :: SpecWith ()
testBound = do
    it "#1"  $ bound varX `shouldBe` empty
    it "#2"  $ bound varX1 `shouldBe` empty
    it "#3"  $ bound lamId `shouldBe` singleton "x"
    it "#4"  $ bound lamK `shouldBe` fromList ["x", "x1"]
    it "#5"  $ bound lamKK `shouldBe` fromList ["x", "x1"]
    it "#6"  $ bound appXX  `shouldBe` empty
    it "#7"  $ bound (Lam "x" appXX) `shouldBe` singleton "x"
    it "#8"  $ bound (Lam "x" appXX1) `shouldBe` singleton "x"
    it "#9"  $ bound (Lam "x2" appXX1) `shouldBe` singleton "x2"
    it "#10" $ bound (Lam "x2" (Lam "x" appXX1)) `shouldBe` fromList ["x", "x2"]

testAlpha :: SpecWith ()
testAlpha = do
    it "#1" $ alpha lamId (singleton "x1") `shouldBe` lamId
    it "#2" $ alpha lamId (singleton "x") `shouldBe` (Lam "x0" varX0)
    it "#3" $ alpha lamId (fromList ["x", "x0", "x1"]) `shouldBe` (Lam "x2" varX2)
    it "#4" $ alpha lamId (fromList ["x", "x0", "x1"]) `shouldBe` (Lam "x2" varX2)

    let term = (Lam "x2" (Lam "x" appXX1))
    it "#5" $ alpha term (fromList ["x2"]) `shouldBe` (Lam "x0" (Lam "x" (App (Var "x") (Var "x1"))))
    it "#6" $ alpha term (fromList ["x"]) `shouldBe` (Lam "x2" (Lam "x0" (App (Var "x0") (Var "x1"))))
    it "#7" $ alpha term (fromList ["x", "x2"]) `shouldBe` (Lam "x0" (Lam "x0" (App (Var "x0") (Var "x1"))))

testSubs :: SpecWith ()
testSubs = do
    it "#1" $ substitute varX "x" varX `shouldBe` varX
    it "#2" $ substitute varX "x1" varX2 `shouldBe` varX
    it "#3" $ substitute varX "x" varX2 `shouldBe` varX2
    it "#4" $ substitute appXX1 "x" varX2 `shouldBe` App varX2 varX1
    it "#5" $ substitute appXX1 "x1" varX2 `shouldBe` App varX varX2
    it "#6" $ substitute lamId "x" varX2 `shouldBe` lamId
    it "#7" $ substitute lamK "x1" varX2 `shouldBe` lamK
    it "#8" $ substitute (Lam "x" (App varX varX2)) "x2" varX1 `shouldBe` Lam "x" (App varX varX1)
    it "#9" $ substitute (Lam "x" (App varX varX2)) "x2" appXX1 `shouldBe` Lam "x0" (App varX0 appXX1)

testBeta :: SpecWith ()
testBeta = do
    it "#1"  $ beta varX `shouldBe` varX
    it "#2"  $ beta appXX1 `shouldBe` appXX1
    it "#3"  $ beta lamK `shouldBe` lamK
    it "#4"  $ beta lamId `shouldBe` lamId
    it "#5"  $ beta (App lamId varX) `shouldBe` varX
    it "#6"  $ beta (App lamId varX1) `shouldBe` varX1
    it "#7"  $ beta (App lamK varX) `shouldBe` Lam "x1" varX
    it "#8"  $ beta (App lamKK varX) `shouldBe` Lam "x1" varX1
    it "#9"  $ beta (App lamKK varX) `shouldBe` Lam "x1" varX1
    it "#10" $ beta (App (App lamKK varX) appXX1) `shouldBe` App (Lam "x1" varX1) appXX1

testEta :: SpecWith ()
testEta = do
    it "#1" $ eta varX `shouldBe` varX
    it "#2" $ eta appXX1 `shouldBe` appXX1
    it "#3" $ eta (Lam "x" (App varX1 varX1)) `shouldBe`  Lam "x" (App varX1 varX1)
    it "#4" $ eta (Lam "x" (App varX1 varX)) `shouldBe` varX1
    it "#5" $ eta (Lam "x" (App varX varX)) `shouldBe` (Lam "x" (App varX varX))

testReduce :: SpecWith ()
testReduce = do
    it "#1" $ reduce varX `shouldBe` varX
    it "#2" $ reduce appXX1 `shouldBe` appXX1
    it "#3" $ reduce lamId `shouldBe` lamId
    it "#4" $ reduce (App lamK varX) `shouldBe` Lam "x1" varX
    it "#5" $ reduce (App (App lamK varX) varX2) `shouldBe` varX
    it "#6" $ reduce (Lam "x0" (App (App (App lamK varX) varX2) varX0)) `shouldBe` varX


testEqual :: SpecWith ()
testEqual = do
  it "#1" $ (Lam "x" varX) `shouldBe` (Lam "y" $ Var "y")
  it "#2" $ (reduce (App lamK lamId)) `shouldBe` lamKK   ---- K I = K*
