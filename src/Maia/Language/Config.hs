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

import Data.Singletons
import Maia.Language.Cardinality
import Data.Kind

data ArgConfig where
  NoArg :: ArgConfig
  Arg :: Type -> ArgConfig

data instance Sing (c :: ArgConfig) where
  SNoArg :: Sing NoArg
  SArg :: Ord a => Sing (Arg a)

instance SingI NoArg where sing = SNoArg
instance Ord a => SingI (Arg a) where sing = SArg

type NoArg = 'NoArg
type Arg = 'Arg



data ErrConfig where
  NoErr :: ErrConfig
  Err :: Type -> ErrConfig

data instance Sing (e :: ErrConfig) where
  SNoErr :: Sing NoErr
  SErr :: Sing (Err e)

instance SingI NoErr where sing = SNoErr
instance SingI (Err e) where sing = SErr

type NoErr = 'NoErr
type Err = 'Err



data Config where
  ConfigIs :: Cardinality -> ArgConfig -> ErrConfig -> Config

type ConfigIs = 'ConfigIs

type DefaultConfig = ConfigIs One NoArg NoErr

data instance Sing (c :: Config) where
  SConfig :: Sing card -> Sing arg -> Sing err -> Sing (ConfigIs card arg err)

instance (SingI args, SingI card, SingI err) => SingI (ConfigIs args card err) where
  sing = SConfig sing sing sing
