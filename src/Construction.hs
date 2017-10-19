module Construction
  ( Name, Term(..)
  , bound, free, fresh
  , reduce, substitute, alpha, beta, eta
  ) where

import           Construction.Internal.Functions (alpha, beta, bound, eta, free,
                                                  fresh, reduce, substitute)
import           Construction.Internal.Types     (Name, Term(..))
