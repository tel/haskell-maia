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
import Maia.Internal.Lens
import Maia.Internal.Provided
import Maia.Language
import Maia.Language.Config
import Maia.Language.Cardinality
import Maia.Record

type family RespOf f where
  RespOf (Field (Config card args e) (Atomic a)) =
    ArgsFor args (ErrorValue e (CollectionOf card a))

  RespOf (Field (Config card args e) (Nested t)) =
    ArgsFor args (ErrorValue e (CollectionOf card (Response t)))

type family ArgsFor args a where
  ArgsFor NoArg a = Provided a
  ArgsFor (Arg arg) a = Map arg a

newtype Resp f =
  Resp (RespOf f)

newtype Response t =
  Response (Rec Resp (Fields t))

lResponse' :: Lens' (Response t) (Rec Resp (Fields t))
lResponse' inj (Response rc) = Response <$> inj rc
