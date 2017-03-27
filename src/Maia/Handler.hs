-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Maia.Handler where

import Maia.Language
import Maia.Language.Named
import Maia.Language.Config
import Maia.Language.Cardinality
import Data.Vinyl

newtype Handling f s =
  Handling (HandlerFor f (NamedValue s))

type family HandlerFor f s where
  HandlerFor f (Field (Config card arg err) (Atomic a)) =
    ArgFor arg (f (ErrorFor err (CollectionOf card a)))
  HandlerFor f (Field (Config card arg err) (Nested t)) =
    ArgFor arg (f (ErrorFor err (CollectionOf card (Handler f t))))

type family ArgFor args a where
  ArgFor NoArg a = a
  ArgFor (Arg arg) a = arg -> a

type family ErrorFor err a where
  ErrorFor NoErr a = a
  ErrorFor (Err e) a = Either e a

newtype Handler f t =
  Handler { handlerRecord :: Rec (Handling f) (Fields t) }
  deriving (Show)
