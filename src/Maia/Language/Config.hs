-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Maia.Language.Config where

import Data.Kind
import Data.Proxy
import Data.Singletons
import Maia.Language.Cardinality

data ArgConfig where
  NoArg :: ArgConfig
  Arg :: Type -> ArgConfig

data instance Sing (c :: ArgConfig) where
  SNoArg :: Sing NoArg
  SArg :: Ord a => Proxy a -> Sing (Arg a)

instance SingI NoArg where sing = SNoArg
instance Ord a => SingI (Arg a) where sing = SArg Proxy

type NoArg = 'NoArg
type Arg = 'Arg



data ErrConfig where
  NoErr :: ErrConfig
  Err :: Type -> ErrConfig

data instance Sing (e :: ErrConfig) where
  SNoErr :: Sing NoErr
  SErr :: Proxy e -> Sing (Err e)

instance SingI NoErr where sing = SNoErr
instance SingI (Err e) where sing = SErr Proxy

type NoErr = 'NoErr
type Err = 'Err

type family ErrorValue err a where
  ErrorValue NoErr a = a
  ErrorValue (Err e) a = Either e a


data FieldConfig where
  Config :: Cardinality -> ArgConfig -> ErrConfig -> FieldConfig

type Config = 'Config

type DefaultConfig = Config One NoArg NoErr

data instance Sing (c :: FieldConfig) where
  SConfig :: Sing card -> Sing arg -> Sing err -> Sing (Config card arg err)

instance (SingI args, SingI card, SingI err) => SingI (Config args card err) where
  sing = SConfig sing sing sing
