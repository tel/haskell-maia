-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ApplicativeDo #-}

module Example where

import Data.Proxy
import Maia.Language
import Maia.Lookup
import Maia.Lookup.Builder
import Maia.Request
import Maia.Response

import Data.Vinyl

data Location

type instance Fields Location =
  [ "latitude" :- Atomic Double
  , "longitude" :- Atomic Double
  ]

latitude :*: longitude :*: End =
  lookups (Proxy :: Proxy Location)

data City

type instance Fields City =
  [ "name" :- Atomic String
  , "coordinates" :- Nested Location
  , "mayor" :- Nested Person
  ]

name :*: Nesting coordinates :*: mayor :*: End =
  lookups (Proxy :: Proxy City)

data Person

type instance Fields Person =
  [ "firstName" :- Atomic String
  , "lastName" :- Atomic String
  , "hometown" :- Nested City
  ]

firstName :*: lastName :*: Nesting hometown :*: End =
  lookups (Proxy :: Proxy Person)

--------------------------------------------------------------------------------

lk1 = (,) <$> latitude <*> longitude

locResp :: Response Location
locResp = Response $
     Resp (Just 0.0)
  :& Resp Nothing
  :& RNil

lk2 = do
  n <- name
  (lat, long) <- coordinates $
    (,) <$> latitude <*> longitude
  return (lat, long, n)

cityResp :: Response City
cityResp = Response $
     Resp (Just "Atlanta")
  :& Resp (Just locResp)
  :& Resp Nothing
  :& RNil
