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

import Data.Singletons
import Data.Singletons.Prelude hiding (Lookup, (:-))
import Data.Vinyl
import Data.Void
import GHC.TypeLits
import Maia.Internal.Lens
import Maia.Language
import Maia.Language.Cardinality
import Maia.Language.Config
import Maia.Lookup
import Maia.Request
import Maia.Response as Response hiding (ArgsFor)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Maia.Lookup.Error as Le

data End = End
data a :*: b = a :*: b
infixr 2 :*:

newtype Nesting e card t t' =
  Nesting { runNesting :: forall r . Lookup t' e r -> Lookup t e (CollectionOf card r) }

newtype Query t s = Query (QueryOf t s)

type family QueryOf t s = l where
  QueryOf t '(ConfigIs card args err, Atomic a) =
    ArgsFor args (Lookup t (ErrorFor err) (CollectionOf card a))
  QueryOf t '(ConfigIs card args err, Nested t') =
    ArgsFor args (Nesting (ErrorFor err) card t t')

type family ArgsFor args r where
  ArgsFor NoArg r = r
  ArgsFor (Arg arg) r = arg -> r

type family ErrorFor e where
  ErrorFor NoErr = Void
  ErrorFor (Err e) = e

type family Lookups t rs = r where
  Lookups t '[] = End
  Lookups t ( (n :- s) ': rs) = Query t s :*: Lookups t rs

type FieldLens t n s = forall f . Lens' (Rec f (Fields t)) (f (n :- s))

lookups :: forall t proxy . SingI (Fields t) => proxy t -> Lookups t (Fields t)
lookups _ = go (sing :: Sing (Fields t)) id where
  go :: forall rs . Sing rs -> RecSel (Fields t) rs -> Lookups t rs
  go SNil _ = End
  go (SCons (SNamed (n :: Sing n) (s :: Sing s)) rs') l =
    lookupOne (symbolVal n) mempty (l . rlHead) s :*: go rs' (l . rlTail)

lookupOne ::
  String -> Request t -> FieldLens t n s -> Sing s -> Query t s
lookupOne name req0 lens (STuple2 c SAtomic) =
  lookupAtomic name req0 lens c
lookupOne name req0 lens (STuple2 c (SNested fields)) =
  lookupNested name req0 lens c fields

handleError ::
  Sing e -> Response.ErrorsFor e a ->
  (a -> Result (ErrorFor e) r) -> Result (ErrorFor e) r
handleError SNoErr a k = k a
handleError SErr (Left e) _ = Result (Left (Le.LocalError e))
handleError SErr (Right a) k = k a

lookupAtomic ::
  forall t c n a .
  String -> Request t -> FieldLens t n '(c, Atomic a) ->
  Sing c -> Query t '(c, Atomic a)
lookupAtomic name req0 lens c =
  case c of
    SConfig (_ :: Sing card) SNoArg (e :: Sing e) ->
      let
        req :: Request t
        req = lset (lRequest' . lens) (Req True) req0
        resph :: Response t -> Result (ErrorFor e) (CollectionOf card a)
        resph resp = case lget (lResponse' . lens) resp of
          Resp Nothing ->
            Result (Left (Le.BadResponse (Le.MissingKey name)))
          Resp (Just res) ->
            handleError e res (Result . Right)
      in Query (Lookup req resph)

    SConfig (_ :: Sing card) SArg (e :: Sing e) ->
      Query $ \i ->
        let
          req :: Request t
          req = lset (lRequest' . lens) (Req (Set.singleton i)) req0
          resph :: Response t -> Result (ErrorFor e) (CollectionOf card a)
          resph resp = case lget (lResponse' . lens) resp of
            Resp mp -> case Map.lookup i mp of
              Nothing ->
                Result (Left (Le.BadResponse (Le.MissingKey name)))
              Just res ->
                handleError e res (Result . Right)
        in Lookup req resph

lookupNested ::
  forall t n c t' .
  String -> Request t -> FieldLens t n '(c, Nested t') ->
  Sing c -> Sing (Fields t') -> Query t '(c, Nested t')
lookupNested name req0 lens c _ = case c of
  SConfig (card :: Sing card) SNoArg (e :: Sing e) ->
    Query $ Nesting $ \(subLk :: Lookup t' (ErrorFor e) r) ->
      let
        req :: Request t
        req = lset (lRequest' . lens) (Req (Just (request subLk))) req0
        resph :: Response t -> Result (ErrorFor e) (CollectionOf card r)
        resph resp = case lget (lResponse' . lens) resp of
          Resp Nothing ->
            Result (Left (Le.BadResponse (Le.MissingKey name)))
          Resp (Just res) ->
            handleError e res (traverseColl card (responseHandler subLk))
      in Lookup req resph

  SConfig (card :: Sing card) SArg (e :: Sing e) ->
    Query $ \i ->
      Nesting $ \(subLk :: Lookup t' (ErrorFor e) r) ->
        let
          req :: Request t
          req = lset (lRequest' . lens) (Req (Map.singleton i (request subLk))) req0
          resph :: Response t -> Result (ErrorFor e) (CollectionOf card r)
          resph resp = case lget (lResponse' . lens) resp of
            Resp mp -> case Map.lookup i mp of 
              Nothing ->
                Result (Left (Le.BadResponse (Le.MissingKey name)))
              Just res ->
                handleError e res (traverseColl card (responseHandler subLk))
        in Lookup req resph
