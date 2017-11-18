{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec
import qualified Text.Parsec as TP (parse)
import Text.Parsec.Text
import Data.Text
import Tasks

main :: IO ()
main = hspec $ do
    describe "Task 1 test" parserTest
    describe "My tests" myTest
    describe "Hardcore tests" myHardcoreTest

parserTest :: SpecWith ()
parserTest = do
  it "#1" $ check parse "42" $ i 42
  it "#2" $ check parse "(42) " $ i 42
  it "#3" $ check parse "(42 ) " $ i 42
  it "#4" $ check parse "(   42 ) " $ i 42
  it "#5" $ check parse "(  T ) " $ b True
  it "#6" $ check parse "42 + 41" $ i 42 `Add` i 41
  it "#7" $ check parse "42 < 41" $ i 42 `Leq` i 41
  it "#8" $ check parse "(42 < 41)" $ i 42 `Leq` i 41
  it "#9" $ check parse "T" $ b True
  it "#10" $ check parse "F" $ b False
  it "#11" $ check parse "(F)" $ b False
  it "#12" $ check parse "T && T" $ b True `And` b True

  it "#13" $ checkWithEval parse "T && (100 < 42)" $ BLit False
  it "#14" $ checkWithEval parse "T && F" $ BLit False
  it "#15" $ checkWithEval parse "3 < 4" $ BLit True
  it "#16" $ checkWithEval parse "3 + 42" $ ILit 45
  it "#17" $ checkWithEval parse "3 + 2 + 1" $ ILit 6
  it "#18" $ checkWithEval parse "(3 + 2) + 1" $ ILit 6
  it "#19" $ checkWithEval parse "3 + 42 < 43" $ BLit False
  it "#20" $ checkWithEval parse "42 + 2 < 49 && T" $ BLit True

myTest :: SpecWith ()
myTest = do
  it "1.1" $ checkWithEval parse "1 < 2 && 2 < 3 && 3 < 4" $ BLit True
  it "1.2" $ checkWithEval parse "2 < 1 && 2 < 3 && 3 < 4" $ BLit False
  it "1.3" $ checkWithEval parse "1 < 2 && 3 < 2 && 3 < 4" $ BLit False
  it "1.4" $ checkWithEval parse "1 < 2 && 2 < 3 && 4 < 3" $ BLit False
  it "2.1" $ checkWithEval parse "(1 < 2 && 2 < 3) && 3 < 4" $ BLit True
  it "2.2" $ checkWithEval parse "(2 < 1 && 2 < 3) && 3 < 4" $ BLit False
  it "2.3" $ checkWithEval parse "(1 < 2 && 3 < 2) && 3 < 4" $ BLit False
  it "2.4" $ checkWithEval parse "(1 < 2 && 2 < 3) && 4 < 3" $ BLit False
  it "3.1" $ checkWithEval parse "1 < 2 && (2 < 3 && 3 < 4)" $ BLit True
  it "3.2" $ checkWithEval parse "2 < 1 && (2 < 3 && 3 < 4)" $ BLit False
  it "3.3" $ checkWithEval parse "1 < 2 && (3 < 2 && 3 < 4)" $ BLit False
  it "3.4" $ checkWithEval parse "1 < 2 && (2 < 3 && 4 < 3)" $ BLit False
  it "4.1" $ checkWithEval parse "8 + 20 + 200" $ ILit 228
  it "4.2" $ checkWithEval parse "(8 + 20) + 200" $ ILit 228
  it "4.3" $ checkWithEval parse "8 + (20 + 200)" $ ILit 228

myHardcoreTest :: SpecWith ()
myHardcoreTest = do
  it "1.1" $ checkWithEval parse "1 < 1+1 && 1+1 < 1+1+1 && 1+1+1 < 1+1+1+1" $ BLit True
  it "1.2" $ checkWithEval parse "1+1 < 1 && 1+1 < 1+1+1 && 1+1+1 < 1+1+1+1" $ BLit False
  it "1.3" $ checkWithEval parse "1 < 1+1 && 1+1+1 < 1+1 && 1+1+1 < 1+1+1+1" $ BLit False
  it "1.4" $ checkWithEval parse "1 < 1+1 && 1+1 < 1+1+1 && 1+1+1+1 < 1+1+1" $ BLit False
  it "2.1" $ checkWithEval parse "(1 < 1+1 && 1+1 < 1+1+1) && 1+1+1 < 1+1+1+1" $ BLit True
  it "2.2" $ checkWithEval parse "(1+1 < 1 && 1+1 < 1+1+1) && 1+1+1 < 1+1+1+1" $ BLit False
  it "2.3" $ checkWithEval parse "(1 < 1+1 && 1+1+1 < 1+1) && 1+1+1 < 1+1+1+1" $ BLit False
  it "2.4" $ checkWithEval parse "(1 < 1+1 && 1+1 < 1+1+1) && 1+1+1+1 < 1+1+1" $ BLit False
  it "3.1" $ checkWithEval parse "1 < 1+1 && (1+1 < 1+1+1 && 1+1+1 < 1+1+1+1)" $ BLit True
  it "3.2" $ checkWithEval parse "1+1 < 1 && (1+1 < 1+1+1 && 1+1+1 < 1+1+1+1)" $ BLit False
  it "3.3" $ checkWithEval parse "1 < 1+1 && (1+1+1 < 1+1 && 1+1+1 < 1+1+1+1)" $ BLit False
  it "3.4" $ checkWithEval parse "1 < 1+1 && (1+1 < 1+1+1 && 1+1+1+1 < 1+1+1)" $ BLit False
  it "no space 1.1" $ checkWithEval parse "1<1+1&&1+1<1+1+1&&1+1+1<1+1+1+1" $ BLit True
  it "no space 1.2" $ checkWithEval parse "1+1<1&&1+1<1+1+1&&1+1+1<1+1+1+1" $ BLit False
  it "no space 1.3" $ checkWithEval parse "1<1+1&&1+1+1<1+1&&1+1+1<1+1+1+1" $ BLit False
  it "no space 1.4" $ checkWithEval parse "1<1+1&&1+1<1+1+1&&1+1+1+1<1+1+1" $ BLit False
  it "no space 2.1" $ checkWithEval parse "(1<1+1&&1+1<1+1+1)&&1+1+1<1+1+1+1" $ BLit True
  it "no space 2.2" $ checkWithEval parse "(1+1<1&&1+1<1+1+1)&&1+1+1<1+1+1+1" $ BLit False
  it "no space 2.3" $ checkWithEval parse "(1<1+1&&1+1+1<1+1)&&1+1+1<1+1+1+1" $ BLit False
  it "no space 2.4" $ checkWithEval parse "(1<1+1&&1+1<1+1+1)&&1+1+1+1<1+1+1" $ BLit False
  it "no space 3.1" $ checkWithEval parse "1<1+1&&(1+1<1+1+1&&1+1+1<1+1+1+1)" $ BLit True
  it "no space 3.2" $ checkWithEval parse "1+1<1&&(1+1<1+1+1&&1+1+1<1+1+1+1)" $ BLit False
  it "no space 3.3" $ checkWithEval parse "1<1+1&&(1+1+1<1+1&&1+1+1<1+1+1+1)" $ BLit False
  it "no space 3.4" $ checkWithEval parse "1<1+1&&(1+1<1+1+1&&1+1+1+1<1+1+1)" $ BLit False
  it "spaced 1.1" $ checkWithEval parse " 1 < 1  +  1  &&  1  +  1 < 1  +  1  +  1  &&  1  +  1  +  1 < 1  +  1  +  1  +  1 " $ BLit True
  it "spaced 1.2" $ checkWithEval parse " 1  +  1 < 1  &&  1  +  1 < 1  +  1  +  1  &&  1  +  1  +  1 < 1  +  1  +  1  +  1 " $ BLit False
  it "spaced 1.3" $ checkWithEval parse " 1 < 1  +  1  &&  1  +  1  +  1 < 1  +  1  &&  1  +  1  +  1 < 1  +  1  +  1  +  1 " $ BLit False
  it "spaced 1.4" $ checkWithEval parse " 1 < 1  +  1  &&  1  +  1 < 1  +  1  +  1  &&  1  +  1  +  1  +  1 < 1  +  1  +  1 " $ BLit False
  it "spaced 2.1" $ checkWithEval parse " ( 1 < 1  +  1  &&  1  +  1 < 1  +  1  +  1 )   &&  1  +  1  +  1 < 1  +  1  +  1  +  1 " $ BLit True
  it "spaced 2.2" $ checkWithEval parse " ( 1  +  1 < 1  &&  1  +  1 < 1  +  1  +  1 )   &&  1  +  1  +  1 < 1  +  1  +  1  +  1 " $ BLit False
  it "spaced 2.3" $ checkWithEval parse " ( 1 < 1  +  1  &&  1  +  1  +  1 < 1  +  1 )   &&  1  +  1  +  1 < 1  +  1  +  1  +  1 " $ BLit False
  it "spaced 2.4" $ checkWithEval parse " ( 1 < 1  +  1  &&  1  +  1 < 1  +  1  +  1 )   &&  1  +  1  +  1  +  1 < 1  +  1  +  1 " $ BLit False
  it "spaced 3.1" $ checkWithEval parse " 1 < 1  +  1  &&   ( 1  +  1 < 1  +  1  +  1  &&  1  +  1  +  1 < 1  +  1  +  1  +  1 ) " $ BLit True
  it "spaced 3.2" $ checkWithEval parse " 1  +  1 < 1  &&   ( 1  +  1 < 1  +  1  +  1  &&  1  +  1  +  1 < 1  +  1  +  1  +  1 ) " $ BLit False
  it "spaced 3.3" $ checkWithEval parse " 1 < 1  +  1  &&   ( 1  +  1  +  1 < 1  +  1  &&  1  +  1  +  1 < 1  +  1  +  1  +  1 ) " $ BLit False
  it "spaced 3.4" $ checkWithEval parse " 1 < 1  +  1  &&   ( 1  +  1 < 1  +  1  +  1  &&  1  +  1  +  1  +  1 < 1  +  1  +  1 ) " $ BLit False
  it "brackets 1.1" $ checkWithEval parse "((((((2))) + 3) < 49) && F)" $ BLit False
  it "brackets 1.2" $ checkWithEval parse "((2 < 3)) && F" $ BLit False


check :: (Eq a, Show a) => Parser a -> Text -> a -> Expectation
check parser inputStr result =
  TP.parse parser "parser" inputStr `shouldBe` Right result

checkWithEval :: Show a => Parser (Expr a) -> Text -> Lit a -> Expectation
checkWithEval parser inputStr result =
  expr <$> TP.parse parser "parser" inputStr `shouldBe` Right result
