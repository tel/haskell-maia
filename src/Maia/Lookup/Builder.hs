-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Maia.Lookup.Builder where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Singletons
import Data.Singletons.Prelude hiding (Lookup, (:-))
import Data.Vinyl
import Data.Void
import GHC.Exts (Constraint)
import Maia.Internal.Lens
import Maia.Language
import Maia.Lookup
import qualified Maia.Lookup.Error as Le
import Maia.Request
import Maia.Response
import GHC.TypeLits

data End = End
data a :*: b = a :*: b
infixr 2 :*:

newtype Nesting e t t' = Nesting (forall r . Lookup t' e r -> Lookup t e r)

type family Query t s = l | l -> t s where
  Query t (Atomic a) = Lookup t Void a
  Query t (Nested t') = Nesting Void t t'

type family Lookups t rs = r where
  Lookups t '[] = End
  Lookups t ( (n :- s) ': rs) = Query t s :*: Lookups t rs

lookups :: forall t proxy . SingI (Fields t) => proxy t -> Lookups t (Fields t)
lookups _ = go (sing :: Sing (Fields t)) id where
  go :: forall rs . Sing rs -> RecSel (Fields t) rs -> Lookups t rs
  go SNil l = End
  go (SCons (SNamed (n :: Sing n) s) rs') l =
    case s of
      SAtomic (_ :: Sing a) ->
        Lookup req resph :*: go rs' (l . rlTail) where
          target :: Lens' (Rec f (Fields t)) (f (n :- Atomic a))
          target = l . rlHead
          req :: Request t
          req = lset (lRequest' . target) (Req True) mempty
          resph :: Response t -> Result Void a
          resph resp = case lget (lResponse' . target) resp of
            Resp Nothing ->
              Result (Left (Le.BadResponse (Le.MissingKey (symbolVal n))))
            Resp (Just a) ->
              Result (Right a)
      SNested _ (_ :: Sing t') ->
        let
          nested :: Nesting Void t t'
          nested =
            Nesting $ \(lk0 :: Lookup t' Void r) ->
              let
                target :: Lens' (Rec f (Fields t)) (f (n :- Nested t'))
                target = l . rlHead
                req :: Request t
                req = lset (lRequest' . target) (Req (Just (request lk0))) mempty
                resph :: Response t -> Result Void r
                resph resp = case lget (lResponse' . target) resp of
                  Resp Nothing ->
                    Result (Left (Le.BadResponse (Le.MissingKey (symbolVal n))))
                  Resp (Just subResp) ->
                    nestResult (symbolVal n) (responseHandler lk0 subResp)
              in Lookup req resph
        in nested :*: go rs' (l . rlTail)
