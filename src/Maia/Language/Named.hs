-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @Named d@ is the kind of types @d@ with an associated @Symbol@ "naming"
-- that type. For instance, we might have a type level list
--
--    type A = '["foo" :- Int, "bar" :- Char]
--
module Maia.Language.Named where

import Data.Singletons
import GHC.TypeLits

data Named k where
  (:-) :: n -> k -> Named k

type n :- v = n ':- v

data instance Sing (s :: Named v) where
  SNamed :: (s ~ (n :- v), KnownSymbol n) => Sing n -> Sing v -> Sing (n :- v)

instance (KnownSymbol n, SingI n, SingI v) => SingI (n :- v) where
  sing = SNamed sing sing

-- | Extracts the type of kind @k@ from a type of kind @Named k@.
type family NamedValue (n :: Named v) :: v where
  NamedValue (n :- v) = v
