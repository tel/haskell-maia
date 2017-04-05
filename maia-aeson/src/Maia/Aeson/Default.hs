-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Maia.Aeson.Default where

import Data.Aeson.Types
import Data.Singletons.Prelude (SingI (sing), Sing (SNil, SCons))
import GHC.Exts (Constraint)
import Maia.Language
import Maia.Language.Config
import Maia.Record
import Maia.Serialization

type family HasDefaultSerializer rs :: Constraint where
  HasDefaultSerializer '[] = ()
  HasDefaultSerializer (n ':- Field conf ft ': rs) =
    ( HasDefaultSerializer rs
    , HasDefaultSerializer_Target ft
    , HasDefaultSerializer_Config conf
    )

type family HasDefaultSerializer_Target tgt :: Constraint where
  HasDefaultSerializer_Target (Atomic a) = (ToJSON a, FromJSON a)
  HasDefaultSerializer_Target (Nested t) = (HasApi t, DefaultSerializer t)

type family HasDefaultSerializer_Config conf :: Constraint where
  HasDefaultSerializer_Config (Config card NoArg NoErr) =
    ()
  HasDefaultSerializer_Config (Config card (Arg arg) NoErr) =
    (ToJSON arg, FromJSON arg)
  HasDefaultSerializer_Config (Config card NoArg (Err e)) =
    (ToJSON e, FromJSON e)
  HasDefaultSerializer_Config (Config card (Arg arg) (Err e)) =
    (ToJSON arg, FromJSON arg, ToJSON e, FromJSON e)

class HasApi t => DefaultSerializer t where
  defaultSerializer :: Serializer Parser Value t

  default defaultSerializer ::
    HasDefaultSerializer (Fields t) => Serializer Parser Value t
  defaultSerializer = Serializer (defaultSerializerRec sing)

toFromJson :: (ToJSON a, FromJSON a) => Section Parser Value a
toFromJson = Section toJSON parseJSON

defaultSerializerRec ::
  HasDefaultSerializer rs => Sing rs -> Rec (Serializes Parser Value) rs
defaultSerializerRec SNil = RNil
defaultSerializerRec (SCons (SNamed _ (SField conf ft)) tl) =
  here :& defaultSerializerRec tl

  where
    here =
      case ft of
        SAtomic _ ->
          case conf of
            SConfig _ SNoArg SNoErr ->
              Serializes ((), (), toFromJson)
            SConfig _ (SArg _) SNoErr ->
              Serializes (toFromJson, (), toFromJson)
            SConfig _ SNoArg (SErr _) ->
              Serializes ((), toFromJson, toFromJson)
            SConfig _ (SArg _) (SErr _) ->
              Serializes (toFromJson, toFromJson, toFromJson)
        SNested _ ->
          case conf of
            SConfig _ SNoArg SNoErr ->
              Serializes ((), (), defaultSerializer)
            SConfig _ (SArg _) SNoErr ->
              Serializes (toFromJson, (), defaultSerializer)
            SConfig _ SNoArg (SErr _) ->
              Serializes ((), toFromJson, defaultSerializer)
            SConfig _ (SArg _) (SErr _) ->
              Serializes (toFromJson, toFromJson, defaultSerializer)
