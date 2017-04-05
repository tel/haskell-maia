-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The core module defining elements of the Maia type-level API description
-- langauge. Most elements of this are described in submodules of the
-- @Maia.Language.*@ hierarchy, but several types must be defined together in
-- order to resolve circularity.

module Maia.Language where

-- NOTE: We must import instances from Data.Singletons.Prelude in order to
-- define HasApi instances--it's needed for SingI for lists.

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude ()
import           Maia.Language.Config
import           Maia.Record

--------------------------------------------------------------------------------

-- | Any type may participate in the Api description by instantiating @HasApi@
-- and providing a list of @Fields@.

class SingI (Fields t) => HasApi t where
  type Fields t :: [Named FieldSpec]

fieldsSing :: HasApi t => Proxy t -> Sing (Fields t)
fieldsSing _ = sing

--------------------------------------------------------------------------------

-- | A type of kind @FieldSpec@ describes a Maia query. At its simplest, this is
-- merely either an @Atomic@ query or a hook for a @Nested@ query, but
-- @FieldSpec@s may customize their @FieldConfig@s to change the result
-- multiplicity, parameterize the result by an argument, or provide for errors
-- arising at this field.

data FieldSpec where
  Field :: FieldConfig -> Target -> FieldSpec

type Field c t = 'Field c t

data instance Sing (s :: FieldSpec) where
  SField :: s ~ Field c t => Sing c -> Sing t -> Sing (Field c t)

instance (SingI c, SingI t) => SingI (Field c t) where
  sing = SField sing sing

--------------------------------------------------------------------------------

-- | Any Maia query targets either an @Atomic@ type---a Haskell type with
-- known serialization sent and returned wholesale---or a @Nested@ type which
-- allows for a set up Maia subqueries.

data Target where
  Atomic :: Type -> Target
  Nested :: Type -> Target

type Atomic = 'Atomic
type Nested = 'Nested

-- | In the most standard case, we want an @'Atomic@ @FieldSpec@ with the
-- default @FieldConfig@.
type Atomic' a = Field DefaultConfig (Atomic a)

-- | In the second most standard case, we want an @'Nested@ @FieldSpec@ with the
-- default @FieldConfig@.
type Nested' t = Field DefaultConfig (Nested t)

data instance Sing (s :: Target) where
  SAtomic :: (s ~ Atomic a) => Proxy a -> Sing (Atomic a)
  SNested :: (s ~ Nested t, HasApi t) => Proxy t -> Sing (Nested t)

instance SingI (Atomic t) where
  sing = SAtomic Proxy

instance (HasApi t, SingI (Fields t)) => SingI (Nested t) where
  sing = SNested Proxy
