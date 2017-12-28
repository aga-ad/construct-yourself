{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction     (Context (..), Equation, Name, Substitution (..), Term (..), Type (..),
                                  termP, typeP, substitutionP, contextP, compose, u1, u, substitute)
import           Data.Text        hiding (singleton)
import           Tasks
import           Test.Hspec
import qualified Text.Parsec      as TP (parse)
import           Text.Parsec.Text
import           Data.Map
import qualified Data.Set         as S (fromList)

main :: IO ()
main = do
  --print $ Substitution $ fromList [("z", TVar "y"), ("a", TVar "b"), ("c", TVar "d"), ("e", TArr (TVar "a") (TArr (TVar "b") (TVar "c")))]
  --print $ (Lam "x0" (Lam "x" (App (Var "x") (Var "x1"))))
  -- print $ u1 (parseType "b->a->b") (parseType "(g->g)->d")
  hspec $ do
    describe "Context monoid test" contextTest
    describe "Substitution monoid test" substitutionTest
    describe "Type parser test" typeParserTest
    describe "Substitution parser test" substitutionParserTest
    describe "Context parser test" contextParserTest
    describe "Substitution parse->show test" substitutionPSTest
    describe "Context parser->show test" contextPSTest
    describe "Term parser->show test" termPSTest
    describe "Simple unification test" u1Test
    describe "Unification test" uTest


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




parsePls str parser = TP.parse parser "parser" (pack str)
checkP parser inputStr result = parsePls inputStr parser `shouldBe` Right result

itTP input answer = it input $ checkP typeP input answer

typeParserTest :: SpecWith ()
typeParserTest = do
  itTP "t" $ TVar "t"
  itTP "  9t0A0Z0  " $ TVar "9t0A0Z0"
  itTP "(((tt)))" $ TVar "tt"
  itTP "a->b->c" $ TArr (TVar "a") (TArr (TVar "b") (TVar "c"))
  itTP "((((a)->((((b)))->((c))))))" $ TArr (TVar "a") (TArr (TVar "b") (TVar "c"))
  itTP " ( ( ( ( ( a ) -> ( ( ( b ) ) ) ) -> ( ( c ) ) ) ) ) " $ TArr (TArr (TVar "a") (TVar "b")) (TVar "c")
  itTP "(a->b)->(a->c)->(a->d)" $ TArr (TArr (TVar "a") (TVar "b")) (TArr (TArr (TVar "a") (TVar "c")) (TArr (TVar "a") (TVar "d")))

itSP input answer = it input $ checkP substitutionP input answer

substitutionParserTest :: SpecWith ()
substitutionParserTest = do
  itSP "z=y,a=b, c = d , e = a->b->c" $ Substitution $ fromList [("z", TVar "y"), ("a", TVar "b"), ("c", TVar "d"), ("e", TArr (TVar "a") (TArr (TVar "b") (TVar "c")))]

itCP input answer = it input $ checkP contextP input answer

contextParserTest :: SpecWith ()
contextParserTest = do
  itCP "a:z->b, b : q -> z  " $ Context $ fromList [("a", TArr (TVar "z") (TVar "b")), ("b", TArr (TVar "q") (TVar "z"))]


itPSmaker parser input =  it input $ (show (parsePls input parser)) `shouldBe` "Right " ++ input

itSPS = itPSmaker substitutionP
substitutionPSTest :: SpecWith ()
substitutionPSTest = do
  itSPS "x = y"
  itSPS "x = y, y = z"
  itSPS "a = (x -> x) -> x -> x, x = y -> y, y = z"


itCPS = itPSmaker contextP
contextPSTest :: SpecWith ()
contextPSTest = do
  itCPS "x : y"
  itCPS "x : y, y : z"
  itCPS "x : a, y : (b -> b) -> b"

itTPS = itPSmaker termP
termPSTest :: SpecWith ()
termPSTest = do
  itTPS "l0l"
  itTPS "A P P l ic a ti0 n"
  itTPS "A (Valid (Application too))"
  itTPS "l 0 l"
  itTPS "\\x.x"
  itTPS "\\x.\\y.x y x y"
  itTPS "\\x.\\y.x (y x) y"
  itTPS "\\x.\\y.x (y (x y))"
  itTPS "y ((\\x.x) \\a.\\b.(a \\z.z) b)"
  itTPS "x (y ((\\x.x) \\a.\\b.(a \\z.z) b))"
  itTPS "\\x.\\y.x (y ((\\x.x) \\a.\\b.(a \\z.z) b))"
  itTPS "((\\x.x) \\x.x) \\x.x" -- not "(\\x.x) (\\x.x) \\x.x"
  itTPS "((\\x.x) \\x.x) (a b)"
  itTPS "((\\x.x) \\x.x) a"
  itTPS "(\\x.x) (a b c d)"
  itTPS "(\\x.x) a b c d"
  itTPS "\\x.x \\a.b c d"
  itTPS "\\x.x a b \\c.d"
  itTPS "x a b \\c.d"
  itTPS "a b (c d)"

----------------------------------------------------------------------- compose test????

itu1 s1 s2 sres = let (Right (a, b, subs)) = do
                                                t1 <- parsePls s1 typeP
                                                t2 <- parsePls s2 typeP
                                                sub <- parsePls sres substitutionP
                                                return (t1, t2, sub)
                  in it (s1 ++ " " ++ s2 ++ "  =>  " ++ sres) $ do
                                                                  u1 a b `shouldBe` Just subs
                                                                  substitute subs a `shouldBe` substitute subs b
itu1' s1 s2 = let (Right (a, b)) = do
                                      t1 <- parsePls s1 typeP
                                      t2 <- parsePls s2 typeP
                                      return (t1, t2)
              in it (s1 ++ " " ++ s2 ++ "  !!!") (u1 a b `shouldBe` Nothing)

u1Test :: SpecWith ()
u1Test = do
  itu1 "a->b->c" "a->b->c" ""
  itu1 "a->b" "b->a" "b=a"
  itu1 "a->b->c" "c->b->a" "c=a"
  itu1 "b->a->b" "(g->g)->d" "b=g->g, d=a->g->g"
  itu1' "c" "a->b->c"
  itu1' "a->b->a" "a->a"
  -- need more tests

parseEq (s1, s2) = do
                      t1 <- parsePls s1 typeP
                      t2 <- parsePls s2 typeP
                      return (t1, t2)

itu sl sres = let (Right (el, res)) = do
                                        lst <- mapM parseEq sl
                                        sub <- parsePls sres substitutionP
                                        return (lst, sub)
              in it (show sl ++ "  =>  " ++ sres) $ do
                                                      u (S.fromList el) `shouldBe` Just res
                                                      Prelude.map (\(a, b) -> substitute res a) el `shouldBe` Prelude.map (\(a, b) -> substitute res b) el

uTest :: SpecWith ()
uTest = do
  itu [("a", "b"), ("c", "d")] "a=b, c=d"
  itu [("a", "a"), ("c", "c"), ("d", "d")] ""
  itu [("b->a->b", "(g->g)->d"), ("g", "a->a")] "b=(a->a)->a->a, d=a->(a->a)->a->a, g=a->a"
