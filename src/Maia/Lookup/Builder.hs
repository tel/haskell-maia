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
import Data.Vinyl hiding (SField)
import Data.Void
import GHC.TypeLits
import Maia.Internal.Lens
import Maia.Language
import Maia.Language.Cardinality
import Maia.Language.Config
import Maia.Language.Named
import Maia.Lookup
import Maia.Request
import Maia.Response as Response hiding (ArgsFor)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Maia.Lookup.Error as Le

data End = End
data a :*: b = a :*: b
infixr 2 :*:

-- | A value of @Zoom _ _ t t'@ transforms a @Lookup@ on the type @t'@ to
-- one on @t@. Use @runZoom@ to access this "@Lookup@-transformer".
newtype Zoom e card t t' =
  Zoom { runZoom :: forall r . Lookup t' e r -> Lookup t e (CollectionOf card r) }

-- | A value of type @Query t s@ knows how to build @Lookup@s from the fields of
-- @t@ to a particular field @s@. The type of the value inside of a @Query@ depends
-- upon @t@ and @s@. It's easiest to let Haskell infer these types.
newtype Query t s = Query { runQuery :: QueryOf t s }

type family QueryOf t s = l where
  QueryOf t (Field (Config card args err) (Atomic a)) =
    ArgsFor args (Lookup t (BestErrorType err) (CollectionOf card a))
  QueryOf t (Field (Config card args err) (Nested t')) =
    ArgsFor args (Zoom (BestErrorType err) card t t')

type family ArgsFor args r where
  ArgsFor NoArg r = r
  ArgsFor (Arg arg) r = arg -> r

type family BestErrorType e where
  BestErrorType NoErr = Void
  BestErrorType (Err e) = e

type family Lookups t rs = r where
  Lookups t '[] = End
  Lookups t ( (n :- s) ': rs) = Query t s :*: Lookups t rs

type FieldLens t n s = forall f . Lens' (Rec f (Fields t)) (f (n :- s))

-- | Generate a heterogenous list of "query-builder"s for a given type with
-- a specified Fields-set. See examples for syntax.
lookups :: forall t proxy . SingI (Fields t) => proxy t -> Lookups t (Fields t)
lookups _ = go (sing :: Sing (Fields t)) id where
  go :: forall rs . Sing rs -> RecSel (Fields t) rs -> Lookups t rs
  go SNil _ = End
  go (SCons (SNamed n s) rs') l =
    lookupOne (symbolVal n) mempty (l . rlHead) s
    :*:
    go rs' (l . rlTail)

-- | Creates a query for a single field within a Fields type.
lookupOne :: forall t n s . String -> Request t -> FieldLens t n s -> Sing s -> Query t s
lookupOne name req0 lens (SField (SConfig card arg (e :: Sing err)) ft) =
  case (ft, arg) of

    (SAtomic _, SNoArg) ->
      Query (buildLookup (Req True) id (Result . Right))

    (SAtomic _, SArg _) ->
      Query $ \i ->
        buildLookup (Req (Set.singleton i)) (Map.lookup i) (Result . Right)

    (SNested _, SNoArg) ->
      Query $ Zoom $ \subLk ->
        buildLookup
          (Req (Just (request subLk)))
          id
          (traverseColl card (responseHandler subLk))

    (SNested _, SArg _) ->
      Query $ \i ->
        Zoom $ \subLk ->
          buildLookup 
            (Req (Map.singleton i (request subLk)))
            (Map.lookup i)
            (traverseColl card (responseHandler subLk)) 

  where

    -- | Most of the work in building a Lookup is mechanical. We (1) modify the
    -- default request with an appropriate "atomic" new request component placed
    -- wherever our record lens, `lens`, points, (2) build the response handler
    -- by using that same lens to reach into a response and (after interpreting the
    -- response accordingly) handle various error conditions.

    buildLookup ::
      Req (n :- s)
      -> (RespOf s -> Maybe (ErrorValue err a))
      -> (a -> Result (BestErrorType err) r)
      -> Lookup t (BestErrorType err) r
    buildLookup reqHole respPre resultHandler = Lookup req resph where
      req =
        lset (lRequest' . lens) reqHole req0
      resph resp =
        case lget (lResponse' . lens) resp of
          Resp coreResp ->
            nestResult name $ case respPre coreResp of
              Nothing ->
                Result (Left (Le.BadResponse (Le.MissingKey name)))
              Just res -> case e of
                SNoErr ->
                  resultHandler res
                SErr _ -> case res of
                  Left le ->
                    Result (Left (Le.LocalError le))
                  Right a ->
                    resultHandler a
