{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction     (Context (..), Equation, Name, Substitution (..), Term (..), Type (..),
                                  typeP, substitutionP, contextP)
import           Data.Text        hiding (singleton)
import           Tasks
import           Test.Hspec
import qualified Text.Parsec      as TP (parse)
import           Text.Parsec.Text
import           Data.Map

main :: IO ()
main = do
  print $ Substitution $ fromList [("z", TVar "y"), ("a", TVar "b"), ("c", TVar "d"), ("e", TArr (TVar "a") (TArr (TVar "b") (TVar "c")))]
  print $ (Lam "x0" (Lam "x" (App (Var "x") (Var "x1"))))
  hspec $ do
    describe "Context monoid test" contextTest
    describe "Substitution monoid test" substitutionTest
    describe "Type parser test" typeParserTest
    describe "Substitution parser test" substitutionParserTest
    describe "Context parser test" contextParserTest
    describe "Substitution parse->show test" substitutionPSTest
    describe "Context parser->show test" contextPSTest


emptyContext = mempty
aContext = Context $ singleton "a" $ TVar "a"
bContext = Context $ singleton "b" $ TVar "b"
abContext = Context $ singleton "a" (TVar "a") `mappend` singleton "b" (TVar "b")

contextTest :: SpecWith ()
contextTest = do
  it "#1" $ aContext `shouldNotBe` bContext
  it "#2" $ aContext `mappend` bContext `shouldBe` bContext `mappend` aContext
  it "#3" $ aContext `mappend` bContext `mappend` emptyContext `mappend` aContext `mappend` bContext `shouldBe` abContext




emptySubstitution = mempty
aSubstitution = Substitution $ singleton "a" $ TVar "a"
bSubstitution = Substitution $ singleton "b" $ TVar "b"
abSubstitution = Substitution $ singleton "a" (TVar "a") `mappend` singleton "b" (TVar "b")

substitutionTest :: SpecWith ()
substitutionTest = do
  it "#1" $ aSubstitution `shouldNotBe` bSubstitution
  it "#2" $ aSubstitution `mappend` bSubstitution `shouldBe` bSubstitution `mappend` aSubstitution
  it "#3" $ aSubstitution `mappend` bSubstitution `mappend` emptySubstitution `mappend` aSubstitution `mappend` bSubstitution `shouldBe` abSubstitution




checkP :: (Eq a, Show a) => Parser a -> Text -> a -> Expectation
checkP parser inputStr result = TP.parse parser "parser" inputStr `shouldBe` Right result
itTP input answer = it input $ checkP typeP (pack input) answer

typeParserTest :: SpecWith ()
typeParserTest = do
  itTP "t" $ TVar "t"
  itTP "  9t0A0Z0  " $ TVar "9t0A0Z0"
  itTP "(((tt)))" $ TVar "tt"
  itTP "a->b->c" $ TArr (TVar "a") (TArr (TVar "b") (TVar "c"))
  itTP "((((a)->((((b)))->((c))))))" $ TArr (TVar "a") (TArr (TVar "b") (TVar "c"))
  itTP " ( ( ( ( ( a ) -> ( ( ( b ) ) ) ) -> ( ( c ) ) ) ) ) " $ TArr (TArr (TVar "a") (TVar "b")) (TVar "c")
  itTP "(a->b)->(a->c)->(a->d)" $ TArr (TArr (TVar "a") (TVar "b")) (TArr (TArr (TVar "a") (TVar "c")) (TArr (TVar "a") (TVar "d")))

itSP input answer = it input $ checkP substitutionP (pack input) answer

substitutionParserTest :: SpecWith ()
substitutionParserTest = do
  itSP "z=y,a=b, c = d , e = a->b->c" $ Substitution $ fromList [("z", TVar "y"), ("a", TVar "b"), ("c", TVar "d"), ("e", TArr (TVar "a") (TArr (TVar "b") (TVar "c")))]

itCP input answer = it input $ checkP contextP (pack input) answer

contextParserTest :: SpecWith ()
contextParserTest = do
  itCP "a:z->b, b : q -> z  " $ Context $ fromList [("a", TArr (TVar "z") (TVar "b")), ("b", TArr (TVar "q") (TVar "z"))]



itSPS input = it input $ (show (TP.parse substitutionP "parser" (pack input))) `shouldBe` "Right " ++ input

substitutionPSTest :: SpecWith ()
substitutionPSTest = do
  itSPS "x = y"
  itSPS "x = y, y = z"
  itSPS "a = (x -> x) -> x -> x, x = y -> y, y = z"


itCPS input = it input $ (show (TP.parse contextP "parser" (pack input))) `shouldBe` "Right " ++ input

contextPSTest :: SpecWith ()
contextPSTest = do
  itCPS "x : y"
  itCPS "x : y, y : z"
  itCPS "x : a, y : (b -> b) -> b"