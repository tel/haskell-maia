-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Maia.Request where

import Data.Map (Map)
import Data.Set (Set)
import Data.Singletons
import Data.Singletons.Prelude hiding ((:-), Map, All)
import Data.Vinyl
import Maia.Internal.Lens
import Maia.Language
import Maia.Language.Config
import qualified Data.Map as Map
import qualified Data.Set as Set

type family ReqOf f where
  ReqOf '(ConfigIs card NoArg err, Atomic a) = Bool
  ReqOf '(ConfigIs card (Arg arg) err, Atomic a) = Set arg
  ReqOf '(ConfigIs card NoArg err, Nested t) = Maybe (Request t)
  ReqOf '(ConfigIs card (Arg arg) err, Nested t) = Map arg (Request t)

newtype Req f =
  Req (ReqOf (NamedValue f))

deriving instance Show (ReqOf f) => Show (Req (n :- f))

newtype Request t =
  Request { requestRecord :: Rec Req (Fields t) }
  deriving (Show)

instance SingI (Fields t) => Monoid (Request t) where

  mempty = Request (go sing) where

    go :: Sing rs -> Rec Req rs
    go SNil = RNil
    go (SCons (SNamed _ z) rs') = defValue z :& go rs'


  mappend = combReq sing where

combReq :: forall t .
  Sing (Fields t) -> Request t -> Request t -> Request t
combReq sft (Request rec1) (Request rec2) =
  Request (go sft rec1 rec2) where

  go :: forall rs .
    Sing rs ->
    Rec Req rs -> Rec Req rs ->
    Rec Req rs
  go rs r1 r2 =
    case rs of
      SNil -> RNil
      SCons (SNamed _ s) rs' -> case (r1, r2) of
        (rh1 :& rt1, rh2 :& rt2) ->
          combValue s rh1 rh2 :& go rs' rt1 rt2

defValue :: Sing s -> Req (n :- s)
defValue (STuple2 (SConfig _ arg _) v) =
  case (v, arg) of
    (SAtomic, SNoArg) -> Req False
    (SAtomic, SArg) -> Req Set.empty
    (SNested _, SNoArg) -> Req Nothing
    (SNested _, SArg) -> Req Map.empty

combValue :: Sing s -> Req (n :- s) -> Req (n :- s) -> Req (n :- s)
combValue s (Req reqa) (Req reqb) = Req (go s reqa reqb) where
  go :: forall s . Sing s -> ReqOf s -> ReqOf s -> ReqOf s
  go (STuple2 (SConfig _ args _) v) a b =
    case (v, args) of
      (SAtomic, SNoArg) ->
        a || b
      (SAtomic, SArg) ->
        Set.union a b
      (SNested fields, SNoArg) -> case (a, b) of
        (Just aa, Just bb) -> Just (combReq fields aa bb)
        (Just aa, _) -> Just aa
        (_, Just bb) -> Just bb
        (_, _) -> Nothing
      (SNested fields, SArg) ->
        Map.unionWith (combReq fields) a b

lRequest' :: Lens' (Request t) (Rec Req (Fields t))
lRequest' inj (Request rc) = Request <$> inj rc
