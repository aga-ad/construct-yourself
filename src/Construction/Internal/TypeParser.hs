module Construction.Internal.TypeParser (typeP) where

import           Construction.Internal.Types (Type (..), Name)
import           Data.Text                   (pack)
import           Text.Parsec.Char            (char, digit, space, alphaNum, string)
import           Text.Parsec.Combinator      (between, many1)
import           Text.Parsec.Prim            (many, try, (<|>))
import           Text.Parsec.Text            (Parser)



typeP :: Parser Type
typeP = try arrowP <|> try nameP <|> bracketP

arrowP :: Parser Type
arrowP = TArr <$> ((try nameP <|> bracketP) <* string "->") <*> typeP

nameP :: Parser Type
nameP = (\x -> TVar $ pack x) <$> (many space *> many1 alphaNum) <* many space

bracketP :: Parser Type
bracketP = many space *> (between (char '(') (char ')') typeP) <* many space
