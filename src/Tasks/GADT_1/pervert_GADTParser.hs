{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tasks.GADT_1.GADTParser where

import           Data.Text              (pack)
import           Tasks.GADT_1.GADTExpr
import           Text.Parsec.Char       (char, digit, oneOf, space, string)
import           Text.Parsec.Combinator (between, many1, chainl1, eof)
import           Text.Parsec.Language   (haskellDef)
import           Text.Parsec.Prim       (many, parseTest, try, (<|>))
import           Text.Parsec.Text       (Parser)
import           Text.Parsec.Token

iLitP :: Parser (Lit Int)
iLitP = (\s -> ILit $ read s) <$> many1 digit

bLitP :: Parser (Lit Bool)
bLitP = (\c -> BLit $ c == 'T') <$> oneOf "TF"



iiLitP :: Parser (Expr Int)
iiLitP = Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = Lit <$> bLitP

addP :: Parser (Expr Int)
addP = Add <$> ((makeParse 1) <* char '+') <*> parse

leqP :: Parser (Expr Bool)
leqP = Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = And <$> ((makeParse 2) <* string "&&") <*> parse

terms = [I iiLitP 0, B bbLitP 0, I addP 1, B leqP 1, B andP 2]


bracketP :: Parser a -> Parser a
bracketP = between (char '(') (char ')')

tsP :: Parser a -> Parser a
tsP p = try $ spacedP p where
  spacedP :: Parser a -> Parser a
  spacedP p = (many space *> p) <* many space



data Term = I (Parser (Expr Int)) Int | B (Parser (Expr Bool)) Int
maxRank = 1000



class MyParse a where
  parse :: Parser (Expr a)
  parse = makeParse maxRank

  getParsers :: [Term] -> Int -> [Parser (Expr a)]

  makeParse :: Int -> Parser (Expr a)
  makeParse limit = unfolderP $ ([tsP] <*> lp) ++ ([\p -> tsP $ bracketP $ tsP p] <*> allp) where
    lp = reverse $ getParsers terms limit
    allp = reverse $ getParsers terms maxRank
    unfolderP lp = foldl (<|>) (head lp) (tail lp)


instance MyParse Int where
  getParsers ((I p rank):xs) limit | rank < limit = p:(getParsers xs limit)
                                   | otherwise = getParsers xs limit
  getParsers (_:xs) limit = getParsers xs limit
  getParsers [] _ = []

instance MyParse Bool where
  getParsers ((B p rank):xs) limit | rank < limit = p:(getParsers xs limit)
                                   | otherwise = getParsers xs limit
  getParsers (_:xs) limit = getParsers xs limit
  getParsers [] _ = []
