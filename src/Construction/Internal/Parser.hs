module Construction.Internal.Parser where

import           Construction.Internal.Types (Term (..), Name)
import           Data.Text                   (pack)
import           Text.Parsec.Char            (char, digit, space, alphaNum)
import           Text.Parsec.Combinator      (between, many1)
import           Text.Parsec.Prim            (many, try, (<|>))
import           Text.Parsec.Text            (Parser)



termP :: Parser Term
termP = varP <|> appP <|> lamP <|> bracketP

varP :: Parser Term
varP =  Var <$> nameP

appP :: Parser Term
appP = try $ between (char '(') (char ')') $
       App <$> (termP <* many1 space)
           <*> termP

bracketP :: Parser Term
bracketP = try $ between (char '(') (char ')') $ termP

lamP :: Parser Term
lamP = try $ between (char '(') (char ')') $
       Lam <$> ((char '\\') *> nameP) <* (char '.') <*> termP


nameP :: Parser Name
nameP = pack <$> many1 alphaNum
