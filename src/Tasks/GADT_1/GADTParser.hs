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
addP = Add <$> ((makeParser [iiLitP]) <* char '+') <*> parse

leqP :: Parser (Expr Bool)
leqP = Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = And <$> ((makeParser [bbLitP, leqP]) <* string "&&") <*> parse

bracketP :: Parser a -> Parser a
bracketP = between (char '(') (char ')')

tsP :: Parser a -> Parser a
tsP p = try $ spacedP p where
  spacedP :: Parser a -> Parser a
  spacedP p = (many space *> p) <* many space

unfolderP :: [Parser a] -> Parser a
unfolderP lp = foldl (<|>) (head lp) (tail lp)



class MyParse a where
  parse :: Parser (Expr a)
  parse = makeParser funcs
  funcs :: [Parser (Expr a)]

  makeParser :: [Parser (Expr a)] -> Parser (Expr a)
  makeParser lp = unfolderP $ ([tsP] <*> lp) ++ ([\p -> tsP $ bracketP $ tsP p] <*> funcs)

instance MyParse Int where
  funcs = [addP, iiLitP]
instance MyParse Bool where
  funcs = [andP, leqP, bbLitP]
