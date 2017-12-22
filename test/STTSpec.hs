{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction     (Context (..), Equation, Name,
                                   Substitution (..), Term (..), Type (..), typeP, substitutionP)
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
    describe "Substitution parser test" substitutionParserTest


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




checkTP :: (Eq a, Show a) => Parser a -> Text -> a -> Expectation
checkTP parser inputStr result = TP.parse parser "parser" inputStr `shouldBe` Right result
itTP input answer = it input $ checkTP typeP (pack input) answer

typeParserTest :: SpecWith ()
typeParserTest = do
  itTP "t" $ TVar "t"
  itTP "  9t0A0Z0  " $ TVar "9t0A0Z0"
  itTP "(((tt)))" $ TVar "tt"
  itTP "a->b->c" $ TArr (TVar "a") (TArr (TVar "b") (TVar "c"))
  itTP "((((a)->((((b)))->((c))))))" $ TArr (TVar "a") (TArr (TVar "b") (TVar "c"))
  itTP " ( ( ( ( ( a ) -> ( ( ( b ) ) ) ) -> ( ( c ) ) ) ) ) " $ TArr (TArr (TVar "a") (TVar "b")) (TVar "c")
  itTP "(a->b)->(a->c)->(a->d)" $ TArr (TArr (TVar "a") (TVar "b")) (TArr (TArr (TVar "a") (TVar "c")) (TArr (TVar "a") (TVar "d")))

itSP input answer = it input $ checkTP substitutionP (pack input) answer

substitutionParserTest :: SpecWith ()
substitutionParserTest = do
  itSP "z=y,a=b, c = d , e = a->b->c" $ Substitution $ fromList [("z", TVar "y"), ("a", TVar "b"), ("c", TVar "d"), ("e", TArr (TVar "a") (TArr (TVar "b") (TVar "c")))]
