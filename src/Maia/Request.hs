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

import Control.Applicative
import Data.Singletons
import Data.Singletons.Prelude hiding ((:-))
import Data.Vinyl
import GHC.Exts (Constraint)
import Maia.Internal.Lens
import Maia.Language

type family ReqOf (f :: Field) where
  ReqOf (Atomic a) = Bool
  ReqOf (Nested t) = Maybe (Request t)

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
    go (SCons (SNamed _ z) rs') =
      case z of
        SAtomic _ -> Req False :& go rs'
        SNested _ _ -> Req Nothing :& go rs'
  mappend = goReq sing where

    goReq :: forall t .
      Sing (Fields t) -> Request t -> Request t -> Request t
    goReq s (Request r1) (Request r2) = Request (go s r1 r2)

    go :: forall rs .
      Sing rs ->
      Rec Req rs -> Rec Req rs ->
      Rec Req rs
    go rs r1 r2 =
      case rs of
        SNil -> RNil
        SCons (SNamed _ (SAtomic a)) rs' -> case (r1, r2) of
          (Req rh1 :& rt1, Req rh2 :& rt2) ->
            Req (rh1 || rh2) :& go rs' rt1 rt2
        SCons (SNamed (_ :: Sing n) (SNested sfields (_ :: Sing t'))) rs' ->
          case (r1, r2) of
            (Req rh1 :& rt1, Req rh2 :& rt2) ->
              let
                req' :: Maybe (Request t')
                req' = case (rh1, rh2) of
                  (Just a, Just b) -> Just (goReq sfields a b)
                  (Just a, _) -> Just a
                  (_, b) -> b
              in Req req' :& go rs' rt1 rt2

lRequest' :: Lens' (Request t) (Rec Req (Fields t))
lRequest' inj (Request rc) = Request <$> inj rc
