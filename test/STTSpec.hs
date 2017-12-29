{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction     (Context (..), Equation, Name, Substitution (..), Term (..), Type (..),
                                  termP, typeP, substitutionP, contextP, setEquationP, compose, u, substitute, e, pp)
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
  --print $ e (parsePls "x:t1, y:t2" contextP) (parsePls "\\a.a x y" termP) (parsePls "t" typeP)
  {-print $ do cont <- parsePls "x:t1, y:t2" contextP
             term <- parsePls "\\a.a x y" termP
             tpe <- parsePls "t" typeP
             return (e cont term tpe)
  print $ do cont <- parsePls "" contextP
             term <- parsePls "\\x.\\y.x y" termP
             tpe <- parsePls "t" typeP
             return (e cont term tpe) -}
  print $ do cont <- parsePls "x:a1->a3, y:a2, z:a1->a2" contextP
             term <- parsePls "(\\a.a) (\\a.a x) (\\a.a)" termP
             tpe <- parsePls "t" typeP
             return $ do ee <- e cont term tpe
                         return (ee, u ee)

  print $ pp (unRight (parsePls "(\\a.a) (\\a.a) (\\a.a)" termP))

  -- print $ do s <- parsePls "x=a, y=b" substitutionP
  --            t <- parsePls "a=t1, b=t2" substitutionP
  --            return (compose s t)

  hspec $ do
    describe "Context monoid test" contextTest
    describe "Substitution monoid test" substitutionTest
    describe "Type parser test" typeParserTest
    describe "Substitution parser test" substitutionParserTest
    describe "Context parser test" contextParserTest
    describe "Substitution parse->show test" substitutionPSTest
    describe "Context parser->show test" contextPSTest
    describe "Term parser->show test" termPSTest
    describe "Context substitution test" contextSubstitutionTest
    describe "Type substitution test" typeSubstitutionTest
    describe "Compose test" composeTest
    describe "Unification test" uTest
    describe "Equatations test" eTest

unRight (Right a) = a
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


itCS ssbst sctx sres = let (Right (sbst, ctx, res)) = do sbst <- parsePls ssbst substitutionP
                                                         ctx <- parsePls sctx contextP
                                                         res <- parsePls sres contextP
                                                         return (sbst, ctx, res)
                       in it (sctx ++ " substitute " ++ ssbst ++ " => " ++ sres) $ (substitute sbst ctx) `shouldBe` res

contextSubstitutionTest :: SpecWith ()
contextSubstitutionTest = do
  itCS "" "x:t1, y:t2, z:t2->t1" "x:t1, y:t2, z:t2->t1"
  itCS "t1=a1, t2=a2->a2" "" ""
  itCS "t1=a1" "x:t1" "x:a1"
  itCS "t1=a1, t2=a2->a2" "x:t1, y:t2, z:t2->t1" "x:a1, y:a2->a2, z:(a2->a2)->a1"
  itCS "t2=a2->a2" "x:t1, y:t2, z:t2->t1" "x:t1, y:a2->a2, z:(a2->a2)->t1"


itTS ssbst st sres = let (Right (sbst, t, res)) = do sbst <- parsePls ssbst substitutionP
                                                     t <- parsePls st typeP
                                                     res <- parsePls sres typeP
                                                     return (sbst, t, res)
                     in it (st ++ " substitute " ++ ssbst ++ " => " ++ sres) $ (substitute sbst t) `shouldBe` res

typeSubstitutionTest :: SpecWith ()
typeSubstitutionTest = do
  itTS "" "t1->t2->(t3->t4)->t5" "t1->t2->(t3->t4)->t5"
  itTS "t1=a, t2=b" "t" "t"
  itTS "t1=a->a, t2=b->b->b" "t1->t2" "(a->a)->b->b->b"
  itTS "t1=a->a, t2=b->b->b, t4=a" "t1->(t2->t3)->t4" "(a->a)->((b->b->b)->t3)->a"
  itTS "t1=t1, t2=t2, t4=t4, t3=t3, t5=t5" "(t1->(t2->t3))->t4" "(t1->(t2->t3))->t4"

itCompose ss st sres = let (Right (s, t, res)) = do s <- parsePls ss substitutionP
                                                    t <- parsePls st substitutionP
                                                    res <- parsePls sres substitutionP
                                                    return (s, t, res)
                       in it (st ++ " ◦ " ++ ss ++ " == " ++ sres) $ (compose s t) `shouldBe` res
composeTest :: SpecWith ()
composeTest = do
  itCompose "a=b" "b=c" "a=c, b=c"
  itCompose "a=b, x=y" "b=c" "a=c, b=c, x=y"
  itCompose "" "" ""
  itCompose "a=x2" "" "a=x2"
  itCompose "" "a=x2" "a=x2"
  itCompose "a=x->y->x, b=(y->y)->x->z" "x=q->q, y=q->q, z=q" "x=q->q, y=q->q, z=q, a=(q->q)->(q->q)->q->q, b=((q->q)->q->q)->(q->q)->q"


parseEq (s1, s2) = do t1 <- parsePls s1 typeP
                      t2 <- parsePls s2 typeP
                      return (t1, t2)

itu sl sres = let (Right (el, res)) = do lst <- mapM parseEq sl
                                         sub <- parsePls sres substitutionP
                                         return (lst, sub)
              in it (show sl ++ "  =>  " ++ sres) $ do u (S.fromList el) `shouldBe` Just res
                                                       Prelude.map (\(a, b) -> substitute res a) el `shouldBe` Prelude.map (\(a, b) -> substitute res b) el

itu' sl = let (Right el) = mapM parseEq sl
          in it (show sl ++ "  !!!") $ u (S.fromList el) `shouldBe` Nothing

uTest :: SpecWith ()
uTest = do
  itu [("a->b->c", "a->b->c")] ""
  itu [("a->b", "b->a")] "a=b"
  itu [("a->b->c", "c->b->a")] "a=c"
  itu [("b->a->b", "(g->g)->d")] "b=g->g, d=a->g->g"
  itu' [("c", "a->b->c")]
  itu' [("a->b->a", "a->a")]
  itu [("a", "b"), ("c", "d")] "a=b, c=d"
  itu [("a", "a"), ("c", "c"), ("d", "d")] ""
  itu [("b->a->b", "(g->g)->d"), ("g", "a->a")] "b=(a->a)->a->a, d=a->(a->a)->a->a, g=a->a"

ite sctx sterm st sres = let (Right (ctx, term, t, res)) = do s <- parsePls sctx contextP
                                                              term <- parsePls sterm termP
                                                              t <- parsePls st typeP
                                                              res <- parsePls sres setEquationP
                                                              return (s, term, t, res)
                        in it (sctx ++ " |- " ++ sterm ++ " : " ++ st) $ (e ctx term t) `shouldBe` (Just res)

eTest :: SpecWith ()
eTest = do
  ite "x:t1" "x" "t" "t==t1"
  ite "x:a1, y:a2" "\\a.a x y" "t" "t==x0->x1, x2==a2, x3==a1, x3->x2->x1==x0"
  ite "x:a1->a3, y:a2, z:a1->a2" "\\a.a x y" "t->t0" "x2==a2, x3==a1->a3, t->t0==x0->x1, x3->x2->x1==x0"
  ite "x:a1->a3, y:a2, z:a1->a2" "(\\a.a) (\\a.a) (\\a.a)" "t" "x0 == x6 -> x7,x1 == x4 -> x5,x3 == x2,x5 == x4,x7 == x6,x1 -> x0 -> t == x2 -> x3"
  ite "x:a1->a3, y:a2, z:a1->a2" "(\\a.a) (\\a.a x) (\\a.a)" "t" "x0 == x7 -> x8,x1 == x4 -> x5,x3 == x2,x6 == a1 -> a3,x8 == x7,x1 -> x0 -> t == x2 -> x3,x6 -> x5 == x4]"
