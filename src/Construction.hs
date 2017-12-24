module Construction
  ( Name, Term(..), Type (..),
    Context (..), Substitution (..),
    Equation
  , bound, free, fresh
  , reduce, substitute, alpha, beta, eta
  , termP, varP, appP, lamP, bracketP
  , typeP, substitutionP, contextP
  ) where

import           Construction.Internal.Functions (alpha, beta, bound, eta, free,
                                                  fresh, reduce, substitute)
import           Construction.Internal.Parser    (appP, bracketP, lamP, termP,
                                                  varP)
import           Construction.Internal.Types     (Name, Term(..), Type (..),
                                                  Context (..), Substitution (..),
                                                  Equation)
import           Construction.Internal.TypeParser (typeP, substitutionP, contextP)
