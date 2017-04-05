-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeFamilies #-}

module Maia.Serialization where

import Maia.Record
import Maia.Language
import Maia.Language.Config

-- | A @Section f v a@ demonstrates that values of type @a@ can be embedded into
-- @v@ via (@inject@) and some values of @v@ can be interpreted as values of @a@
-- (via @retract@). Retraction returns in an effect context @f@ allowing for
-- failure.
data Section f b a
  = Section
  { inject  :: a -> b
  , retract :: b -> f a
  }

bimapSection ::
  Functor f =>
  (b -> y) -> (y -> b) ->
  (x -> a) -> (a -> x) ->
  (Section f b a -> Section f y x)
bimapSection by yb xa ax s =
  Section
  (by . inject s . xa)
  (fmap ax . retract s . yb)

mapSectionTarget ::
  (b -> x) -> (x -> b) ->
  (Section f b a -> Section f x a)
mapSectionTarget bx xb s =
  Section
  (bx . inject s)
  (retract s . xb)

mapSectionSource ::
  Functor f =>
  (x -> a) -> (a -> x) ->
  (Section f b a -> Section f b x)
mapSectionSource xa ax s =
  Section
  (inject s . xa)
  (fmap ax . retract s)

mapSectionWrapper :: (f a -> g a) -> (Section f b a -> Section g b a)
mapSectionWrapper f s = s { retract = f . retract s }

-- | A @Serializer f v t@ describes how to convert to and from the "leaf" types
-- of the @Fields@ of type @t@ and values of type @v@. It's anticipated that @v@
-- is a larger, more general type (like @String@ or @JSONValue@) and so
-- conversions from leaf values to @v@ are guaranteed to succeed and conversions
-- back are performed within the context of @f@ allowing for failure.
newtype Serializer f v t
  = Serializer (Rec (Serializes f v) (Fields t))

newtype Serializes f v s
  = Serializes (SerializerFor f v s)

type family SerializerFor f v s where
  SerializerFor f v (Field (Config card arg e) (Atomic a)) =
    (ArgSerializer f v arg, ErrSerializer f v e, Section f v a)
  SerializerFor f v (Field (Config card arg e) (Nested t)) =
    (ArgSerializer f v arg, ErrSerializer f v e, Serializer f v t)

type family ArgSerializer f v a where
  ArgSerializer f v NoArg = ()
  ArgSerializer f v (Arg a) = Section f v a

type family ErrSerializer f v a where
  ErrSerializer f v NoErr = ()
  ErrSerializer f v (Err a) = Section f v a
