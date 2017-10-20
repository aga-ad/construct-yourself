module Construction
  ( Name, Term(..)
  , bound, free, fresh
  , reduce, substitute, alpha, beta, eta, equals, notEquals
  ) where

import           Construction.Internal.Functions (alpha, beta, bound, eta, free,
                                                  fresh, reduce, substitute, equals, notEquals)
import           Construction.Internal.Types     (Name, Term(..))
