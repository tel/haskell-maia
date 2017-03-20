-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Maia.Response where

import Data.Singletons
import Data.Singletons.Prelude hiding ((:-))
import Data.Vinyl
import Maia.Internal.Lens
import Maia.Language

type family RespOf (f :: Field) where
  RespOf (Atomic a) = Maybe a
  RespOf (Nested t) = Maybe (Response t)

newtype Resp f =
  Resp (RespOf (NamedValue f))

deriving instance Show (RespOf f) => Show (Resp (n :- f))

newtype Response t =
  Response (Rec Resp (Fields t))
  deriving (Show)

lResponse' :: Lens' (Response t) (Rec Resp (Fields t))
lResponse' inj (Response rc) = Response <$> inj rc
