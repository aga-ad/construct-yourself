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

instance MyParse Int where
  funcs = [addP, iiLitP]
instance MyParse Bool where
  funcs = [andP, leqP, bbLitP]


class MyParse a where
  funcs :: [Parser (Expr a)]

  parse :: Parser (Expr a)
  parse = makeParser funcs

  makeParser :: [Parser (Expr a)] -> Parser (Expr a)
  -- makeParser lp = alternativerP $ ([tsP] <*> lp) ++ ([\p -> tsP $ bracketP $ tsP p] <*> funcs) where
  makeParser lp = alternativerP $ ([tsP] <*> lp) ++ ([\p -> tsP $ bracketedP $ tsP p] <*> funcs) where
    bracketP = between (char '(') (char ')')
    bracketedP p = (try $ bracketP $ spacedP p) <|> (bracketP $ spacedP $ bracketedP $ spacedP p)
    alternativerP [x] = x
    alternativerP (x:xs) = (tsP x) <|> alternativerP xs
    tsP p = try $ spacedP p
    spacedP :: Parser a -> Parser a
    spacedP p = (many space *> p) <* many space
