-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import qualified Data.Aeson                 as A
import qualified Data.Aeson.Diff            as Diff
import qualified Data.Aeson.Parser          as A
import qualified Data.Aeson.Types           as A
import qualified Data.ByteString.Lazy       as Sl
import qualified Data.ByteString.Lazy.Char8 as Sl8
import qualified Maia.Aeson                 as Ma
import qualified Maia.Aeson.Default         as Default
import qualified Maia.Example.Api           as Ex
import           Maia.Request
import           Maia.Response
import qualified Maia.Serialization         as Sz
import           Test.HUnit

_requestSerializer :: Default.DefaultSerializer t => Ma.AesonSection (Request t)
_requestSerializer = Ma.requestSerializer Default.defaultSerializer

_responseSerializer :: Default.DefaultSerializer t => Ma.AesonSection (Response t)
_responseSerializer = Ma.responseSerializer Default.defaultSerializer

reqJson :: Default.DefaultSerializer t => Request t -> A.Value
reqJson = Sz.inject _requestSerializer

respJson :: Default.DefaultSerializer t => Response t -> A.Value
respJson = Sz.inject _responseSerializer

readResp :: Default.DefaultSerializer t => Sl.ByteString -> Maybe (Response t)
readResp = A.decodeWith A.json' (A.parse $ Sz.retract _responseSerializer)

readReq :: Default.DefaultSerializer t => Sl.ByteString -> Maybe (Request t)
readReq = A.decodeWith A.json' (A.parse $ Sz.retract _requestSerializer)

parseResp :: Default.DefaultSerializer t => A.Value -> Either String (Response t)
parseResp = A.parseEither $ Sz.retract _responseSerializer

parseReq :: Default.DefaultSerializer t => A.Value -> Either String (Request t)
parseReq = A.parseEither $ Sz.retract _requestSerializer

data SzAssertion t
  = ReqJsonIs (Request t) A.Value
  | RespJsonIs (Response t) A.Value
  | RoundTripReqEquality (Request t)
  | RoundTripRespEquality (Response t)

assertJsonEq :: A.Value -> A.Value -> Assertion
assertJsonEq value value'
  | value == value' = return ()
  | otherwise = do
      let
        astring :: A.ToJSON a => a -> String
        astring = Sl8.unpack . A.encode
      let diff = Diff.diff value value'
      assertFailure (astring diff ++ " --> " ++ astring value ++ " AND " ++ astring value')

instance (Default.DefaultSerializer t, Ex.TestHandler t) => Assertable (SzAssertion t) where
  assert (ReqJsonIs req v) =
    assertJsonEq (reqJson req) v
  assert (RespJsonIs resp v) =
    assertJsonEq (respJson resp) v
  assert (RoundTripReqEquality req) =
    let value = reqJson req
    in case parseReq value of
      Left err -> assertString $ "Did not reparse: " ++ err
      Right (req' :: Request t) ->
        assertJsonEq value (reqJson req')
  assert (RoundTripRespEquality resp) =
    let value = respJson resp
    in case parseResp value of
      Left err -> assertString $ "Did not reparse: " ++ err
      Right (resp' :: Response t) ->
        assertJsonEq value (respJson resp')
