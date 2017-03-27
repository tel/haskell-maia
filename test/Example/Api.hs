-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Example.Api where

import Data.Functor.Identity
import Data.Proxy
import Data.Vinyl
import Maia.Handler
import Maia.Language
import Maia.Language.Cardinality
import Maia.Language.Config
import Maia.Language.Named ((:-))
import Maia.Lookup.Builder

--------------------------------------------------------------------------------

data Location

instance HasApi Location where
  type Fields Location =
    [ "latitude" :- Atomic' Double
    , "longitude" :- Atomic' Double
    ]

locationHandler :: Double -> Double -> Handler Identity Location
locationHandler lat long = Handler $
     Handling (Identity lat)
  :& Handling (Identity long)
  :& RNil

Query latitude :*: Query longitude :*: End =
  lookups (Proxy :: Proxy Location)

data City

instance HasApi City where
  type Fields City =
    [ "name" :- Atomic' String
    , "coordinates" :- Nested' Location
    , "mayor" :- Field (Config Opt NoArg NoErr) (Nested Person)
    ]

atlanta :: Handler Identity City
atlanta = Handler $
     Handling (Identity "Atlanta")
  :& Handling (Identity $ locationHandler 33.7490 84.3880)
  :& Handling (Identity Nothing)
  :& RNil

cityHandler :: String -> Maybe (Handler Identity City)
cityHandler "Atlanta" = Just atlanta
cityHandler _ = Nothing

Query name :*: Query (Zoom coordinates) :*: Query (Zoom mayor) :*: End =
  lookups (Proxy :: Proxy City)

data Person

instance HasApi Person where
  type Fields Person =
    [ "firstName" :- Atomic' String
    , "lastName" :- Atomic' String
    , "hometown" :- Field (Config Opt NoArg NoErr) (Nested City)
    ]

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

Query firstName :*: Query lastName :*: Query (Zoom hometown) :*: End =
  lookups (Proxy :: Proxy Person)

data Api

instance HasApi Api where
  type Fields Api =
    [ "getCity" :- Field (Config Opt (Arg String) NoErr) (Nested City)
    , "getCurrentUser" :- Field (Config Opt NoArg NoErr) (Nested Person)
    ]

Query getUser :*: Query (Zoom getCurrentUser) :*: End =
  lookups (Proxy :: Proxy Api)

apiHandler :: Handler Identity Api
apiHandler = Handler $
     Handling (Identity . cityHandler)
  :& Handling (Identity Nothing) -- not a live system
  :& RNil
