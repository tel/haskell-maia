-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveFunctor #-}

module Maia.Lookup.Error where

data LookupError e
  = Branch (LookupError e) (LookupError e)
  | Nest String (LookupError e)
  | LocalError e
  | BadResponse BadResponseError
  deriving (Eq, Ord, Show, Functor)

data BadResponseError
  = MissingKey String
  deriving (Eq, Ord, Show)
