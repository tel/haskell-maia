-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- |
--
-- "Null collapse" occurs when loose encodings of the Response data structure to
-- JSON allow for ambiguity in presence of three kinds of missingness:
--
-- - missingness arising because the response actually doesn't contain a value
--   for a given field (unrequested data nulls),
-- - missingness arising because the field has a multiplicity of Opt (Opt-nulls),
-- - and nulls arising from actual Maybe's in atomic fields (atomic nulls).
--
-- All three of these must be encoded differently and should not be able to be
-- mistaken for one another.

module Spec.Serializations.NullCollapse where

import           Lib
import qualified Maia.Aeson.Default        as Default
import           Maia.Internal.Provided
import           Maia.Language
import           Maia.Language.Cardinality
import           Maia.Language.Config
import           Maia.Record
import           Maia.Response
import           Test.HUnit
import           Test.Tasty
import qualified Test.Tasty.HUnit          as Hu

--------------------------------------------------------------------------------
-- Examples

data Api

instance HasApi Api where
  type Fields Api =
    '[ "atomNull" :- Atomic' (Maybe Int)
     , "optNull" :- Field (Config Opt NoArg NoErr) (Atomic Int)
     , "combined" :- Field (Config Opt NoArg NoErr) (Atomic (Maybe Int))
     ]

instance Default.DefaultSerializer Api

respAtomNull :: Response Api
respAtomNull =
  Response
  $ Resp (Provided Nothing)
  :& Resp Absent
  :& Resp Absent
  :& RNil

respOptNull :: Response Api
respOptNull =
  Response
  $ Resp Absent
  :& Resp (Provided Nothing)
  :& Resp Absent
  :& RNil

respCombinedNull1 :: Response Api
respCombinedNull1 =
  Response
  $ Resp Absent
  :& Resp Absent
  :& Resp (Provided Nothing)
  :& RNil

respCombinedNull2 :: Response Api
respCombinedNull2 =
  Response
  $ Resp Absent
  :& Resp Absent
  :& Resp (Provided (Just Nothing))
  :& RNil

respMissingNull :: Response Api
respMissingNull =
  Response
  $ Resp Absent
  :& Resp Absent
  :& Resp Absent
  :& RNil

--------------------------------------------------------------------------------
-- Examples

assertUnequal :: (Show a, Eq a) => a -> a -> Assertion
assertUnequal a b
  | a == b = assertFailure ("Expected unequal values: " ++ show a)
  | otherwise = return ()

tests :: TestTree
tests =
  testGroup "Null collapse"
  [ Hu.testCase "stacked nulls unequal"
    $ respJson respCombinedNull1 `assertUnequal` respJson respCombinedNull2
  , Hu.testCase "stacked nulls 1 not like missing"
    $ respJson respCombinedNull1 `assertUnequal` respJson respMissingNull
  , Hu.testCase "stacked nulls 2 not like missing"
    $ respJson respCombinedNull2 `assertUnequal` respJson respMissingNull
  ]
