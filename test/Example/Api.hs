-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Example.Api where

import Data.Proxy
import Maia.Language
import Maia.Language.Config
import Maia.Language.Cardinality
import Maia.Lookup.Builder

data Location

type instance Fields Location =
  [ "latitude" :- Atomic' Double
  , "longitude" :- Atomic' Double
  ]

Query latitude :*: Query longitude :*: End =
  lookups (Proxy :: Proxy Location)

data City

type instance Fields City =
  [ "name" :- Atomic' String
  , "coordinates" :- Nested' Location
  , "mayor" :- '(ConfigIs Opt NoArg NoErr, Nested Person)
  ]

Query name :*: Query (Nesting coordinates) :*: Query (Nesting mayor) :*: End =
  lookups (Proxy :: Proxy City)

data Person

type instance Fields Person =
  [ "firstName" :- Atomic' String
  , "lastName" :- Atomic' String
  , "hometown" :- '(ConfigIs Opt NoArg NoErr, Nested City)
  ]

Query firstName :*: Query lastName :*: Query (Nesting hometown) :*: End =
  lookups (Proxy :: Proxy Person)

data Api

type instance Fields Api =
  [ "getUser" :- '(ConfigIs Opt (Arg Int) NoErr, Nested Person)
  , "getCurrentUser" :- '(ConfigIs Opt NoArg NoErr, Nested Person)
  ]

Query getUser :*: Query (Nesting getCurrentUser) :*: End =
  lookups (Proxy :: Proxy Api)
