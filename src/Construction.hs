module Construction
  ( Name, Term(..), Type (..),
    Context (..), Substitution (..),
    Equation
  , bound, free, fresh
  , reduce, substitute, alpha, beta, eta
  , termP, varP, appP, lamP, bracketP
  , typeP, substitutionP, contextP
  , compose, u1, u
  ) where

import           Construction.Internal.Functions (alpha, beta, bound, eta, free,
                                                  fresh, reduce)
import           Construction.Internal.Parser    (appP, bracketP, lamP, termP,
                                                  varP)
import           Construction.Internal.Types     (Name, Term(..), Type (..),
                                                  Context (..), Substitution (..),
                                                  Equation)
import           Construction.Internal.TypeParser (typeP, substitutionP, contextP)
import           Construction.Internal.TypeFunctions (compose, u1, u, substitute)
