module Construction.Internal.TypeParser (typeP, substitutionP, contextP, setEquationP) where

import           Construction.Internal.Types (Type (..), Name, Substitution (..), Context (..), Equation)
import           Construction.Internal.Parser (termP)
import           Data.Text                   (pack)
import           Data.Map                    (fromList)
import qualified Data.Set               as S (fromList, Set (..))
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
nameP = pack <$> spacedP (many1 alphaNum)

bracketP :: Parser Type
bracketP = spacedP (between (char '(') (char ')') typeP)

spacedP :: Parser a -> Parser a
spacedP p = (many space *> p) <* many space

substitutionP :: Parser Substitution
substitutionP = (\l -> Substitution $ fromList l) <$> sepBy substitution1P (char ',')

substitution1P :: Parser (Name, Type)
substitution1P = (\n t -> (n, t)) <$> (nameP <* (char '=')) <*> typeP

contextP :: Parser Context
contextP = (\l -> Context $ fromList l) <$> sepBy context1P (char ',')

context1P :: Parser (Name, Type)
context1P = (\n t -> (n, t)) <$> (nameP <* (char ':')) <*> typeP

equationP :: Parser Equation
equationP = (\t1 t2 -> (t1, t2)) <$> (typeP <* (string "==")) <*> typeP

setEquationP :: Parser (S.Set Equation)
setEquationP = (\l -> S.fromList l) <$> sepBy equationP (char ',')
