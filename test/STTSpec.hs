{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction     (Context (..), Equation, Name,
                                   Substitution (..), Term (..), Type (..), typeP)
import           Data.Text        hiding (singleton)
import           Tasks
import           Test.Hspec
import qualified Text.Parsec      as TP (parse)
import           Text.Parsec.Text
import           Data.Map

main :: IO ()
main = hspec $ do
    describe "Context monoid test" contextTest
    describe "Substitution monoid test" substitutionTest
    describe "Type parser test" typeParserTest


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


checkTP :: Text -> Type -> Expectation
checkTP inputStr result = TP.parse typeP "parser" inputStr `shouldBe` Right result
itTP input answer = it input $ checkTP (pack input) answer

typeParserTest :: SpecWith ()
typeParserTest = do
  itTP "t" $ TVar "t"
  itTP "  t000  " $ TVar "t000"
  itTP "(((tt)))" $ TVar "tt"
  itTP "a->b->c" $ TArr (TVar "a") (TArr (TVar "b") (TVar "c"))
  itTP "((((a)->((((b)))->((c))))))" $ TArr (TVar "a") (TArr (TVar "b") (TVar "c"))
  itTP " ( ( ( ( ( a ) -> ( ( ( b ) ) ) ) -> ( ( c ) ) ) ) ) " $ TArr (TArr (TVar "a") (TVar "b")) (TVar "c")
  itTP "(a->b)->(a->c)->(a->d)" $ TArr (TArr (TVar "a") (TVar "b")) (TArr (TArr (TVar "a") (TVar "c")) (TArr (TVar "a") (TVar "d")))
