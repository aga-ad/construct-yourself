{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tasks.GADT_1.GADTParser where

import           Data.Text              (pack)
import           Tasks.GADT_1.GADTExpr
import           Text.Parsec.Char       (char, digit, oneOf, space, string)
import           Text.Parsec.Combinator (between, many1)
import           Text.Parsec.Language   (haskellDef)
import           Text.Parsec.Prim       (many, parseTest, try, (<|>))
import           Text.Parsec.Text       (Parser)
import           Text.Parsec.Token

iLitP :: Parser (Lit Int)
iLitP = (\s -> ILit $ read s) <$> many1 digit

bLitP :: Parser (Lit Bool)
bLitP = (\c -> BLit $ c == 'T') <$> oneOf "TF"

iiLitP :: Parser (Expr Int)
iiLitP = try $ spacedP $ Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = try $ spacedP $ Lit <$> bLitP

addP :: Parser (Expr Int)
addP = try $ spacedP $ Add <$> ((iiLitP <|> bracketP iiLitP <|> bracketP addP) <* char '+') <*> parse

leqP :: Parser (Expr Bool)
leqP = try $ spacedP $ Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = try $ spacedP $ And <$> ((bbLitP <|> leqP <|> bracketP bbLitP <|> bracketP leqP <|> bracketP andP) <* string "&&") <*> parse

spacedP :: Parser a -> Parser a
spacedP p = (many space *> p) <* many space

bracketP :: Parser a -> Parser a
bracketP p = try $ spacedP $ between (char '(') (char ')') p

class MyParse a where
  parse :: Parser (Expr a)

instance MyParse Int where
  parse = addP <|> iiLitP <|> bracketP iiLitP <|> bracketP addP
instance MyParse Bool where
  parse = leqP <|> andP <|> bbLitP<|> bracketP leqP <|> bracketP andP <|> bracketP bbLitP
