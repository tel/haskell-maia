-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings         #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Maia.Example.Api where

import qualified Data.Aeson                as A
import           Data.Functor.Identity
import           Data.Proxy
import qualified Maia.Aeson                as A
import qualified Maia.Aeson.Default        as A
import           Maia.Handler
import           Maia.Language
import           Maia.Language.Cardinality
import           Maia.Language.Config
import           Maia.Lookup
import           Maia.Lookup.Builder
import           Maia.Record
import           Maia.Request
import           Maia.Response

--------------------------------------------------------------------------------

-- | Introduced to make testing functions a little easier
class HasApi t => TestHandler t where
  testHandler :: Handler Identity t

runTestHandler :: TestHandler t => Request t -> Response t
runTestHandler = runIdentity . runHandler testHandler

--------------------------------------------------------------------------------

data NonType

instance HasApi NonType where
  type Fields NonType = '[]

instance TestHandler NonType where
  testHandler = Handler RNil

instance A.DefaultSerializer NonType

data Clock

instance HasApi Clock where
  type Fields Clock =
    '[ "getCurrentTime" :- Atomic' Integer ]

instance TestHandler Clock where
  testHandler = clockHandler 0

clockHandler :: Integer -> Handler Identity Clock
clockHandler time = Handler (Handling (Identity time) :& RNil)

instance A.DefaultSerializer Clock

data Location

instance HasApi Location where
  type Fields Location =
    [ "latitude" :- Atomic' Double
    , "longitude" :- Atomic' Double
    ]

instance TestHandler Location where
  testHandler = locationHandler 0 0

locationHandler :: Double -> Double -> Handler Identity Location
locationHandler lat long = Handler $
     Handling (Identity lat)
  :& Handling (Identity long)
  :& RNil

Query latitude :*: Query longitude :*: End =
  lookups (Proxy :: Proxy Location)

instance A.DefaultSerializer Location

data City

instance HasApi City where
  type Fields City =
    [ "name" :- Atomic' String
    , "coordinates" :- Nested' Location
    , "mayor" :- Field (Config Opt NoArg NoErr) (Nested Person)
    ]

instance TestHandler City where
  testHandler = atlanta

atlanta :: Handler Identity City
atlanta = Handler $
     Handling (Identity "Atlanta")
  :& Handling (Identity $ locationHandler 33.7490 84.3880)
  :& Handling (Identity $ Just reedKasim)
  :& RNil

fakeville :: Handler Identity City
fakeville = Handler $
     Handling (Identity "Fakeville")
  :& Handling (Identity $ locationHandler 0 0)
  :& Handling (Identity Nothing)
  :& RNil

cityHandler :: String -> Maybe (Handler Identity City)
cityHandler "Atlanta"   = Just atlanta
cityHandler "Fakeville" = Just fakeville
cityHandler _           = Nothing

Query name :*: Query (SubLookup coordinates) :*: Query (SubLookup mayor) :*: End =
  lookups (Proxy :: Proxy City)

instance A.DefaultSerializer City

data Person

instance HasApi Person where
  type Fields Person =
    [ "firstName" :- Atomic' String
    , "lastName" :- Atomic' String
    , "hometown" :- Field (Config Opt NoArg NoErr) (Nested City)
    ]

instance TestHandler Person where
  testHandler = abrahamsonJoseph

abrahamsonJoseph :: Handler Identity Person
abrahamsonJoseph = Handler $
     Handling (Identity "Joseph")
  :& Handling (Identity "Abrahamson")
  :& Handling (Identity (Just atlanta))
  :& RNil

reedKasim :: Handler Identity Person
reedKasim = Handler $
     Handling (Identity "Kasim")
  :& Handling (Identity "Reed")
  :& Handling (Identity (Just atlanta)) -- essentially...
  :& RNil

Query firstName :*: Query lastName :*: Query (SubLookup hometown) :*: End =
  lookups (Proxy :: Proxy Person)

instance A.DefaultSerializer Person

data Api

data ApiError = NotAuthorized
  deriving (Eq, Ord, Show)

instance A.ToJSON ApiError where
  toJSON NotAuthorized = A.String "NotAuthorized"

instance A.FromJSON ApiError where
  parseJSON = A.withText "ApiError" $ \t ->
    if t == "NotAuthorized"
    then pure NotAuthorized
    else fail "bad parse"

instance HasApi Api where
  type Fields Api =
    [ "getCity" :- Field (Config Opt (Arg String) NoErr) (Nested City)
    , "getAllCities" :- Field (Config Many NoArg (Err ApiError)) (Nested City)
    , "getCurrentUser" :- Field (Config Opt NoArg (Err ApiError)) (Nested Person)
    ]

instance TestHandler Api where
  testHandler = apiHandler

apiHandler :: Handler Identity Api
apiHandler = Handler $
     Handling (Identity . cityHandler)
  :& Handling (Identity (Right [atlanta, fakeville]))
  :& Handling (Identity (Left NotAuthorized))
  :& RNil

Query getCity
  :*: Query (SubLookup getAllCities)
  :*: Query (SubLookup getCurrentUser)
  :*: End =
  lookups (Proxy :: Proxy Api)

instance A.DefaultSerializer Api

apiRequestSerializer :: A.AesonSection (Request Api)
apiRequestSerializer = A.requestSerializer A.defaultSerializer
