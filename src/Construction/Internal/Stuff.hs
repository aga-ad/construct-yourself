module Construction.Internal.Stuff (spp) where

import           Construction     (pp, termP, alpha, bound, Context (..), Term (..), Type (..))
import qualified Text.Parsec      as TP (parse)
import           Data.Text        (pack)
import           Data.Set         (empty)

-- Finds principle pair
spp :: String -> String
spp sterm = case TP.parse termP "parser" (pack sterm) of
  (Right term) -> case pp (alpha term (bound term)) of
    Just (ctx, t) -> show ctx ++ " |- " ++ show term ++ " : " ++ show t
    Nothing -> "pp error"
  (Left s) -> "Parse error :" ++ show s
