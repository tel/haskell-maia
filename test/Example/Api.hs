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
import Maia.Language.Named ((:-))
import Maia.Lookup.Builder

data Location

instance HasApi Location where
  type Fields Location =
    [ "latitude" :- Atomic' Double
    , "longitude" :- Atomic' Double
    ]

Query latitude :*: Query longitude :*: End =
  lookups (Proxy :: Proxy Location)

data City

instance HasApi City where
  type Fields City =
    [ "name" :- Atomic' String
    , "coordinates" :- Nested' Location
    , "mayor" :- Field (Config Opt NoArg NoErr) (Nested Person)
    ]

Query name :*: Query (Zoom coordinates) :*: Query (Zoom mayor) :*: End =
  lookups (Proxy :: Proxy City)

data Person

instance HasApi Person where
  type Fields Person =
    [ "firstName" :- Atomic' String
    , "lastName" :- Atomic' String
    , "hometown" :- Field (Config Opt NoArg NoErr) (Nested City)
    ]

Query firstName :*: Query lastName :*: Query (Zoom hometown) :*: End =
  lookups (Proxy :: Proxy Person)

data Api

instance HasApi Api where
  type Fields Api =
    [ "getUser" :- Field (Config Opt (Arg Int) NoErr) (Nested Person)
    , "getCurrentUser" :- Field (Config Opt NoArg NoErr) (Nested Person)
    ]

Query getUser :*: Query (Zoom getCurrentUser) :*: End =
  lookups (Proxy :: Proxy Api)
