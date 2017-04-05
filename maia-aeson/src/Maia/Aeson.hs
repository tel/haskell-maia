 -- This Source Code Form is subject to the terms of the Mozilla Public
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | Aeson encoders for Maia Request and Response datatypes.

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Maia.Aeson where

import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import           Data.Singletons.Prelude   (Proxy (Proxy), Sing (SCons, SNil),
                                            SingI, sing)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           GHC.TypeLits
import           Maia.Aeson.Default        (toFromJson)
import           Maia.Internal.Provided
import           Maia.Language
import           Maia.Language.Cardinality
import           Maia.Language.Config
import           Maia.Record
import           Maia.Request
import           Maia.Response
import           Maia.Serialization

type AesonSection = Section Parser Value

newtype MaiaError e a
  = MaiaError { runMaiaError :: Either e a }
  deriving (Eq, Show)

instance (ToJSON e, ToJSON a) => ToJSON (MaiaError e a) where
  toJSON (MaiaError (Left e))  = object ["err" .= e]
  toJSON (MaiaError (Right a)) = object ["ok" .= a]

instance (FromJSON e, FromJSON a) => FromJSON (MaiaError e a) where
  parseJSON = withObject "maia error" $ \o -> do
    mayErr <- o .:? "err"
    case mayErr of
      Nothing  -> MaiaError . Right <$> o .: "ok"
      Just err -> pure (MaiaError (Left err))

requestSerializer ::
  SingI (Fields t) => Serializer Parser Value t -> AesonSection (Request t)
requestSerializer (Serializer s0) =
  mapSectionSource (\(Request r) -> r) Request $
  Section (reqBuildWriter sing s0) (reqBuildReader sing s0)

reqBuildWriter ::
  Sing rs -> Rec (Serializes Parser Value) rs ->
  Rec Req rs -> Value
reqBuildWriter rs sz req = toJSON (reqBuildObjWriter rs sz req)

reqBuildObjWriter ::
  Sing rs -> Rec (Serializes Parser Value) rs ->
  Rec Req rs -> Map Text Value
reqBuildObjWriter SNil RNil RNil = Map.empty
reqBuildObjWriter
  (SCons (SNamed n (SField (SConfig _ sarg _) st)) rest)
  (Serializes srh :& srt)
  (Req rqh :& rqt) = reqMap

  where

    name = Text.pack (symbolVal n)
    reqMap0 = reqBuildObjWriter rest srt rqt

    reqMap =
      case st of
        SAtomic _t ->
          let (srarg, _, _) = srh
          in case sarg of
            SNoArg -- rqh is bool
              | rqh == True -> Map.insert name (toJSON True) reqMap0
              | otherwise -> reqMap0
            SArg _a -- rqh is set of args
              | Set.null rqh -> reqMap0
              | otherwise ->
                let inner this tl = inject srarg this : tl
                in Map.insert name (toJSON (foldr inner [] rqh)) reqMap0
        SNested _t ->
          let (srarg, _, Serializer subSr) = srh
          in case sarg of
            SNoArg -> -- rqh is maybe a sub request
              case rqh of
                Absent -> reqMap0
                Provided (Request subReq) ->
                  let subValue = reqBuildWriter sing subSr subReq
                  in Map.insert name subValue reqMap0
            SArg _a -- rqh is a map of sub requests
              | Map.null rqh -> reqMap0
              | otherwise ->
                -- since we're like to support more general args than just
                -- those with nice JSON-key representations we serialize as an
                -- array of argument subrequest pairs.
                let
                  onPair (arg, Request subReq) =
                    (inject srarg arg, reqBuildWriter sing subSr subReq)
                  subReqs = toJSON . map onPair $ Map.toList rqh
                in Map.insert name subReqs reqMap0

reqBuildReader ::
  Sing rs -> Rec (Serializes Parser Value) rs ->
  Value -> Parser (Rec Req rs)
reqBuildReader rs sz = withObject "maia request" (reqBuildObjReader rs sz)

reqBuildObjReader ::
  Sing rs -> Rec (Serializes Parser Value) rs ->
  Object -> Parser (Rec Req rs)
reqBuildObjReader SNil RNil _ = pure RNil
reqBuildObjReader
  (SCons (SNamed n (SField (SConfig _ sarg _) st)) rest)
  (Serializes srh :& srt)
  o = (:&) <$> now <*> later

  where

    name = Text.pack (symbolVal n)
    later = reqBuildObjReader rest srt o

    now =
      case st of
        SAtomic _ ->
          let (srarg, _, _) = srh
          in case sarg of
            SNoArg -> -- expecting a bool
              Req <$> o .:? name .!= False
            SArg _ -> do -- expecting a list of args
              argList <- o .:? name .!= []
              res <- traverse (retract srarg) argList
              pure (Req (Set.fromList res))
        SNested _ ->
          let (srarg, _, Serializer subSr) = srh
          in case sarg of
            SNoArg -> do -- maybe a sub request?
              maySubReq <- o .:? name
              case maySubReq of
                Nothing ->
                  pure (Req Absent)
                Just subReqVal ->
                  Req . Provided . Request <$> reqBuildReader sing subSr subReqVal
            SArg _ -> do -- expecting list of (arg, subReq) pairs
              subReqList <- o .:? name .!= []
              let pairParser v = do
                    (argV, subReqV) <- parseJSON v
                    (,) <$> retract srarg argV
                        <*> (Request <$> reqBuildReader sing subSr subReqV)
              res <- traverse pairParser subReqList
              pure (Req (Map.fromList res))

responseSerializer ::
  SingI (Fields t) => Serializer Parser Value t -> AesonSection (Response t)
responseSerializer (Serializer s0) =
  mapSectionSource (\(Response r) -> r) Response $
  Section (respBuildWriter sing s0) (respBuildReader sing s0)

encodeColl ::
  Sing card -> Section Parser Value a -> Section Parser Value (CollectionOf card a)
encodeColl SOne s = s
encodeColl SOpt s = Section to fro where
  to Nothing  = Null
  to (Just a) = object ["the" .= inject s a]
  fro Null = pure Nothing
  fro (Object o) = do
    val <- o .: "the"
    res <- retract s val
    return (Just res)
  fro _ = fail "Expected null or 'the'-wrapped value"
encodeColl SMany s = Section to fro where
  to = toJSON . map (inject s)
  fro v = do
    valList <- parseJSON v
    traverse (retract s) valList

encodeCollVals :: Sing card -> Section Parser Value (CollectionOf card Value)
encodeCollVals c = encodeColl c (toFromJson :: AesonSection Value)

respBuildWriter ::
  Sing rs -> Rec (Serializes Parser Value) rs -> Rec Resp rs -> Value
respBuildWriter rs sz req = toJSON (respBuildObjWriter rs sz req)

respBuildObjWriter ::
  Sing rs -> Rec (Serializes Parser Value) rs -> Rec Resp rs -> Map Text Value
respBuildObjWriter SNil RNil RNil = Map.empty
respBuildObjWriter
  (SCons (SNamed n (SField (SConfig (scard :: Sing card) sarg serr) st)) rest)
  (Serializes srh :& srt)
  (Resp rqh :& rqt) = respMap

  where

    name = Text.pack (symbolVal n)
    respMap0 = respBuildObjWriter rest srt rqt

    respMap = case st of
      SAtomic _ ->
        let
          (srarg, srerr, srv) = srh
          srcoll = encodeColl scard srv
        in case sarg of
          SNoArg -> -- rqh is direct
            case serr of
              SNoErr -> -- rqh is maybe a value
                case rqh of
                  Absent     -> respMap0
                  Provided v -> Map.insert name (inject srcoll v) respMap0
              SErr _ -> -- rqh is maybe (either an error or a value)
                case rqh of
                  Absent -> respMap0
                  Provided eitv ->
                    let v = toJSON (MaiaError (bimap (inject srerr) (inject srcoll) eitv))
                    in Map.insert name v respMap0
          SArg _ -- rqh is a map of arguments to results
            | Map.null rqh -> respMap0
            | otherwise ->
              case serr of
                SNoErr ->
                  -- each result is just the collection
                  let
                    go (arg, v) = (inject srarg arg, inject srcoll v)
                    result = toJSON (map go (Map.toList rqh))
                  in Map.insert name result respMap0
                SErr _ ->
                  -- each result is either an error or the collection
                  let
                    go (arg, v) =
                      ( inject srarg arg
                      , toJSON (MaiaError (bimap (inject srerr) (inject srcoll) v))
                      )
                    result = toJSON (map go (Map.toList rqh))
                  in Map.insert name result respMap0

      SNested (Proxy :: Proxy t) ->
        let
          (srarg, srerr, Serializer srv) = srh
          respToVal :: Response t -> Value
          respToVal (Response r) = respBuildWriter sing srv r
          collRespsToVal :: CollectionOf card (Response t) -> Value
          collRespsToVal =
            inject (encodeCollVals scard) . mapColl scard respToVal
        in case sarg of
          SNoArg -> -- rqh is direct
            case serr of
              SNoErr -> -- maybe a collection of subresults
                case rqh of
                  Absent -> respMap0
                  Provided subResps -> Map.insert name (collRespsToVal subResps) respMap0
              SErr _ -> -- maybe either an error or a collection of subresults
                case rqh of
                  Absent -> respMap0
                  Provided eitSubResps ->
                    let result =
                          toJSON (MaiaError (bimap (inject srerr) collRespsToVal eitSubResps))
                    in Map.insert name result respMap0
          SArg _ -- rqh is a map of args to subresps
            | Map.null rqh -> respMap0
            | otherwise ->
              case serr of
                SNoErr ->
                  -- each result is just the collection
                  let go (arg, subResps) =
                        (inject srarg arg, collRespsToVal subResps)
                      result =  toJSON (map go (Map.toList rqh))
                  in Map.insert name result respMap0
                SErr _ ->
                  -- each result is just the collection
                  let go (arg, eitSubResps) =
                        ( inject srarg arg
                        , toJSON (MaiaError (bimap (inject srerr) collRespsToVal eitSubResps))
                        )
                      result =  toJSON (map go (Map.toList rqh))
                  in Map.insert name result respMap0

respBuildReader ::
  Sing rs -> Rec (Serializes Parser Value) rs -> Value -> Parser (Rec Resp rs)
respBuildReader rs sz = withObject "maia response" (respBuildObjReader rs sz)

respBuildObjReader ::
  Sing rs -> Rec (Serializes Parser Value) rs ->
  Object -> Parser (Rec Resp rs)
respBuildObjReader SNil RNil _ = pure RNil
respBuildObjReader
  (SCons (SNamed n (SField (SConfig (scard :: Sing card) sarg serr) st)) rest)
  (Serializes srh :& srt)
  o = (:&) <$> now <*> later

  where

    name = Text.pack (symbolVal n)
    later = respBuildObjReader rest srt o

    now =
      case st of
        SAtomic (Proxy :: Proxy a) ->
          let
            (srarg, srerr, srv) = srh
            collAParser :: Value -> Parser (CollectionOf card a)
            collAParser = retract (encodeColl scard srv)
          in case sarg of
            SNoArg -> case serr of
              SNoErr -> do -- expect bare maybe value
                mayValColl <- o .:? name
                case mayValColl of
                  Nothing -> return (Resp Absent)
                  Just valColl -> do
                    result <- collAParser valColl
                    return (Resp (Provided result))
              SErr _ -> do -- expect maybe either mix
                mayValColl <- o .:? name
                case mayValColl of
                  Nothing -> return (Resp Absent)
                  Just (MaiaError (Left errV)) ->
                    Resp . Provided . Left <$> retract srerr errV
                  Just (MaiaError (Right resV)) -> do
                    result <- collAParser resV
                    return (Resp (Provided (Right result)))
            SArg (Proxy :: Proxy arg) -> case serr of
              SNoErr -> do
                mayArgsList <- o .:? name
                case mayArgsList of
                  Nothing -> return (Resp Map.empty)
                  Just (argsList :: [(Value, Value)]) -> do
                    let
                      go :: (Value, Value) -> Parser (arg, CollectionOf card a)
                      go (argV, valsV) = do
                        arg <- retract srarg argV
                        vals <- collAParser valsV
                        return (arg, vals)
                    tups <- traverse go argsList
                    return (Resp (Map.fromList tups))
              SErr (Proxy :: Proxy err) -> do
                mayArgsList <- o .:? name
                case mayArgsList of
                  Nothing -> return (Resp Map.empty)
                  Just (argsList :: [(Value, MaiaError Value Value)]) -> do
                    let
                      go ::
                        (Value, MaiaError Value Value) ->
                        Parser (arg, Either err (CollectionOf card a))
                      go (argV, eitValsV) = do
                        arg <- retract srarg argV
                        vals <- case eitValsV of
                          MaiaError (Left eV) -> Left <$> retract srerr eV
                          MaiaError (Right collV) -> Right <$> collAParser collV
                        return (arg, vals)
                    maps <- traverse go argsList
                    return (Resp (Map.fromList maps))

        SNested (Proxy :: Proxy t) ->
          let
            (srarg, srerr, Serializer srv) = srh
            respParser :: Value -> Parser (Response t)
            respParser =
              fmap Response . respBuildReader sing srv
            respCollParser :: Value -> Parser (CollectionOf card (Response t))
            respCollParser v = do
              valsColl <- retract (encodeCollVals scard) v
              traverseColl scard respParser valsColl
          in case sarg of
            SNoArg -> case serr of
              SNoErr -> do
                maySubObjColl <- o .:? name
                case maySubObjColl of
                  Nothing -> return (Resp Absent)
                  Just subObjColl -> do
                    result <- respCollParser subObjColl
                    return (Resp (Provided result))
              SErr _ -> do
                maySubObjColl <- o .:? name
                case maySubObjColl of
                  Nothing -> return (Resp Absent)
                  Just (MaiaError (Left errV)) ->
                    Resp . Provided . Left <$> retract srerr errV
                  Just (MaiaError (Right resV)) -> do
                    result <- respCollParser resV
                    return (Resp (Provided (Right result)))
            SArg (Proxy :: Proxy arg) -> case serr of
              SNoErr -> do
                mayArgsList <- o .:? name
                case mayArgsList of
                  Nothing -> return (Resp Map.empty)
                  Just (argsList :: [(Value, Value)]) -> do
                    let
                      go ::
                        (Value, Value) -> Parser (arg, CollectionOf card (Response t))
                      go (argV, valsV) = do
                        arg <- retract srarg argV
                        vals <- respCollParser valsV
                        return (arg, vals)
                    tups <- traverse go argsList
                    return (Resp (Map.fromList tups))
              SErr (Proxy :: Proxy err) -> do
                mayArgsList <- o .:? name
                case mayArgsList of
                  Nothing -> return (Resp Map.empty)
                  Just (argsList :: [(Value, MaiaError Value Value)]) -> do
                    let
                      go ::
                        (Value, MaiaError Value Value) ->
                        Parser (arg, Either err (CollectionOf card (Response t)))
                      go (argV, eitValsV) = do
                        arg <- retract srarg argV
                        vals <- case eitValsV of
                          MaiaError (Left eV) ->
                            Left <$> retract srerr eV
                          MaiaError (Right collV) ->
                            Right <$> respCollParser collV
                        return (arg, vals)
                    maps <- traverse go argsList
                    return (Resp (Map.fromList maps))
