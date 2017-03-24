-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Maia.Language where

import Data.Singletons
import Data.Kind
import GHC.TypeLits
import Maia.Language.Config

data Field where
  Atomic :: Type -> Field
  Nested :: Type -> Field

type Atomic = 'Atomic
type Nested = 'Nested

type Atomic' a = '(DefaultConfig, Atomic a)
type Nested' t = '(DefaultConfig, Nested t)

data instance Sing (s :: Field) where
  SAtomic :: s ~ Atomic t => Sing (Atomic t)
  SNested :: s ~ Nested t => Sing (Fields t) -> Sing (Nested t)

instance SingI (Atomic t) where
  sing = SAtomic

instance SingI (Fields t) => SingI (Nested t) where
  sing = SNested sing

data Named k where
  (:-) :: n -> k -> Named k

type n :- v = n ':- v

data instance Sing (s :: Named v) where
  SNamed :: (s ~ (n :- v), KnownSymbol n) => Sing n -> Sing v -> Sing (n :- v)

instance (KnownSymbol n, SingI n, SingI v) => SingI (n :- v) where
  sing = SNamed sing sing

type family NamedValue (n :: Named v) :: v where
  NamedValue (n :- v) = v

type family Fields t :: [Named (Config, Field)]
