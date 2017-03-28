-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Maia.Handler where

import Data.Singletons.Prelude hiding ((:-))
import Maia.Language
import Maia.Language.Cardinality
import Maia.Language.Config
import Maia.Record
import Maia.Request
import Maia.Response
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Handling f s =
  Handling (HandlerFor f s)

type family HandlerFor f s where
  HandlerFor f (Field (Config card arg err) (Atomic a)) =
    ArgFor arg (f (ErrorValue err (CollectionOf card a)))
  HandlerFor f (Field (Config card arg err) (Nested t)) =
    ArgFor arg (f (ErrorValue err (CollectionOf card (Handler f t))))

type family ArgFor args a where
  ArgFor NoArg a = a
  ArgFor (Arg arg) a = arg -> a

newtype Handler f t =
  Handler { handlerRecord :: Rec (Handling f) (Fields t) }

runHandler ::
  (SingI (Fields t), Monad f) => Handler f t -> Request t -> f (Response t)
runHandler (Handler h) (Request r) = fmap Response (runHandler' sing h r)

runHandler' ::
  Monad f =>
  Sing rs ->
  Rec (Handling f) rs ->
  Rec Req rs ->
  f (Rec Resp rs)
runHandler' SNil _ _ = pure RNil
runHandler' (SCons (SNamed _ (SField c t)) rest) (Handling h :& hs) (Req req :& reqs) =
  do
    hd <- handleOne c t h req
    tl <- runHandler' rest hs reqs
    pure (Resp hd :& tl)

-- TODO: Refactor handleOne to something less utterly repetitive.

handleOne ::
  forall f c t . 
  Monad f
  => Sing c
  -> Sing t
  -> HandlerFor f (Field c t)
  -> ReqOf (Field c t)
  -> f (RespOf (Field c t))
handleOne (SConfig _ SNoArg _) (SAtomic _) getAtom wantsResponse =
  if wantsResponse
  then Just <$> getAtom
  else pure Nothing
handleOne (SConfig _ (SArg _) _) (SAtomic _) buildResp argSet =
  Set.foldl' comb (pure Map.empty) argSet where
    comb fmp arg = Map.insert arg <$> buildResp arg <*> fmp
handleOne (SConfig card SNoArg SNoErr) (SNested (pt :: Proxy t')) buildSubHandlers maySubRequest =
  case maySubRequest of
    Nothing ->
      pure Nothing
    Just (Request subr) -> do
      subhs <- buildSubHandlers
      let
        -- NOTE: This requires an explicit annotation to plumb type information
        -- forward and unify on t'; without it the non-injectivity of Fields
        -- will ruin inference.
        go :: Handler f t' -> f (Response t')
        go (Handler subh) =
          Response <$> runHandler' (fieldsSing pt) subh subr
      res <- traverseColl card go subhs
      pure (Just res)
handleOne (SConfig card SNoArg (SErr _)) (SNested (pt :: Proxy t')) buildSubHandlers maySubRequest =
  case maySubRequest of
    Nothing ->
      pure Nothing
    Just (Request subr) -> do
      eit_subhs <- buildSubHandlers
      case eit_subhs of
        Left err ->
          pure (Just (Left err))
        Right subhs -> do
          let
            go :: Handler f t' -> f (Response t')
            go (Handler subh) =
              Response <$> runHandler' (fieldsSing pt) subh subr
          res <- traverseColl card go subhs
          pure (Just (Right res))
handleOne
  (SConfig (card :: Sing card) (SArg (_ :: Proxy arg)) SNoErr)
  (SNested (pt :: Proxy t'))
  buildSubHandlers
  subRequestMap = do
    Map.traverseWithKey eachSubH subRequestMap
    where
      eachSubH :: arg -> Request t' -> f (CollectionOf card (Response t'))
      eachSubH arg (Request subReq) = do
        subHs <- buildSubHandlers arg
        let
          go :: Handler f t' -> f (Response t')
          go (Handler subh) =
            Response <$> runHandler' (fieldsSing pt) subh subReq
        traverseColl card go subHs
handleOne
  (SConfig card (SArg _) (SErr _))
  (SNested (pt :: Proxy t'))
  buildSubHandlers
  subRequestMap = do
    Map.traverseWithKey eachSubH subRequestMap
    where
      eachSubH arg (Request subReq) = do
        eit_subhs <- buildSubHandlers arg
        case eit_subhs of
          Left err ->
            pure (Left err)
          Right subhs ->
            let
              go :: Handler f t' -> f (Response t')
              go (Handler subh) =
                Response <$> runHandler' (fieldsSing pt) subh subReq
            in Right <$> traverseColl card go subhs
