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
{-# LANGUAGE UndecidableInstances #-}

module Maia.Response where

import Data.Map (Map)
import Data.Vinyl
import Maia.Internal.Lens
import Maia.Language
import Maia.Language.Config
import Maia.Language.Cardinality
import Maia.Language.Named

type family RespOf f where
  RespOf (Field (Config card args e) (Atomic a)) =
    ArgsFor args (ErrorsFor e (CollectionOf card a))

  RespOf (Field (Config card args e) (Nested t)) =
    ArgsFor args (ErrorsFor e (CollectionOf card (Response t)))

type family ArgsFor args a where
  ArgsFor NoArg a = Maybe a
  ArgsFor (Arg arg) a = Map arg a

type family ErrorsFor err a where
  ErrorsFor NoErr a = a
  ErrorsFor (Err e) a = Either e a

newtype Resp f =
  Resp (RespOf (NamedValue f))

deriving instance Show (RespOf f) => Show (Resp (n :- f))

newtype Response t =
  Response (Rec Resp (Fields t))
  deriving (Show)

lResponse' :: Lens' (Response t) (Rec Resp (Fields t))
lResponse' inj (Response rc) = Response <$> inj rc
