module Construction.Internal.TypeParser (typeP, substitutionP) where

import           Construction.Internal.Types (Type (..), Name, Substitution (..), Context (..))
import           Data.Text                   (pack)
import           Data.Map                    (fromList)
import           Text.Parsec.Char            (char, digit, space, alphaNum, string)
import           Text.Parsec.Combinator      (between, many1, sepBy)
import           Text.Parsec.Prim            (many, try, (<|>))
import           Text.Parsec.Text            (Parser)



typeP :: Parser Type
typeP = try arrowP <|> try varP <|> bracketP

arrowP :: Parser Type
arrowP = TArr <$> ((try varP <|> bracketP) <* string "->") <*> typeP

varP :: Parser Type
varP = TVar <$> nameP

nameP :: Parser Name
nameP = pack <$> (many space *> many1 alphaNum) <* many space

bracketP :: Parser Type
bracketP = many space *> (between (char '(') (char ')') typeP) <* many space


substitutionP :: Parser Substitution
substitutionP = (\l -> Substitution $ fromList l) <$> sepBy substitution1P (char ',')

substitution1P :: Parser (Name, Type)
substitution1P = (\n t -> (n, t)) <$> (nameP <* (char '=')) <*> typeP
