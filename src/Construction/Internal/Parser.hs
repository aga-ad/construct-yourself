module Construction.Internal.Parser where

import           Construction.Internal.Types (Term (..), Name)
import           Data.Text                   (pack)
import           Text.Parsec.Char            (char, digit, space, alphaNum)
import           Text.Parsec.Combinator      (between, many1, chainl1)
import           Text.Parsec.Prim            (many, try, (<|>))
import           Text.Parsec.Text            (Parser)



termP :: Parser Term
termP = try appP <|> try lamP <|> try varP <|> bracketP -- Be care with spaces.

varP :: Parser Term
varP =  Var <$> nameP

appP :: Parser Term
--appP = try $ between (char '(') (char ')') $
--       App <$> (termP <* many1 space)
--           <*> termP

-- An application term must not be in brackets. Chainl1 function helps to eliminate left recursion.
appP = chainl1 (try lamP <|> try varP <|> bracketP) (many1 space >> return (\a b -> App a b))


bracketP :: Parser Term
bracketP = between (char '(') (char ')') $ termP

lamP :: Parser Term
--lamP = try $ between (char '(') (char ')') $
--       Lam <$> ((char '\\') *> nameP) <* (char '.') <*> termP
lamP = Lam <$> ((char '\\') *> nameP) <* (char '.') <*> termP -- A lambda term must not be in brackets


nameP :: Parser Name
nameP = pack <$> many1 alphaNum

spacedP :: Parser a -> Parser a
spacedP p = (many space *> p) <* many space
