-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Serializations where

import           Data.Aeson
import           Lib
import qualified Maia.Aeson.Default               as Default
import qualified Maia.Example.Api                 as Ex
import           Maia.Lookup
import           Maia.Request
import           Maia.Response
import qualified Spec.Serializations.NullCollapse as NullCollapse
import           Test.HUnit
import           Test.Tasty
import qualified Test.Tasty.HUnit                 as Hu

tests :: TestTree
tests =
  testGroup "Serializations"
  [ requestSerializations
  , NullCollapse.tests
  ]

-- Exercises anticipated serialization and printer/parser loop
szTest ::
  forall t e r .
  (Ex.TestHandler t, Default.DefaultSerializer t) =>
  String ->
  Lookup t e r -> Value -> Value -> TestTree
szTest nm lk expectedReq expectedResp =
  let
    req :: Request t
    req = request lk
    resp :: Response t
    resp = Ex.runTestHandler req
  in testGroup nm
     [ Hu.testCase "request sz" (assert (ReqJsonIs req expectedReq))
     , Hu.testCase "response sz" (assert (RespJsonIs resp expectedResp))
     , Hu.testCase "request parses" (assert (RoundTripReqEquality req))
     , Hu.testCase "response parses" (assert (RoundTripRespEquality resp))
     ]

requestSerializations :: TestTree
requestSerializations =
  testGroup "Lookup requests"
  [ testGroup "Of NonType"
    [ let
        lk :: Lookup' Ex.NonType ()
        lk = pure ()
        exReq = object []
        exResp = object []
      in szTest "unit" lk exReq exResp
    ]

  , testGroup "Of Location"
    [ let
        lk = Ex.latitude
        exReq = object ["latitude" .= True]
        exResp = object ["latitude" .= Number 0.0]
      in szTest "latitude" lk exReq exResp

    , let
        lk = Ex.longitude
        exReq = object ["longitude" .= True]
        exResp = object ["longitude" .= Number 0.0]
      in szTest "longitude" lk exReq exResp

    , let
        lk = (,) <$> Ex.latitude <*> Ex.longitude
        exReq = object ["longitude" .= True, "latitude" .= True]
        exResp = object ["longitude" .= Number 0.0, "latitude" .= Number 0.0]
      in szTest "both" lk exReq exResp
    ]

  , testGroup "Of City"
    [ let
        lk = Ex.name
        exReq = object ["name" .= True]
        exResp = object ["name" .= String "Atlanta"]
      in szTest "name" lk exReq exResp

    , let
        lk = Ex.coordinates (pure ())
        exReq = object ["coordinates" .= object []]
        exResp = object ["coordinates" .= object []]
      in szTest "coordinates, empty" lk exReq exResp

    , let
        lk = Ex.coordinates Ex.latitude
        exReq = object ["coordinates" .= object ["latitude" .= True]]
        exResp = object ["coordinates" .= object ["latitude" .= Number 33.749]]
      in szTest "coordinates, latitude" lk exReq exResp

    , let
        lk = Ex.coordinates ((,) <$> Ex.latitude <*> Ex.longitude)
        exReq = object [ "coordinates" .=
                         object [ "latitude" .= True
                                , "longitude" .= True
                                ]]
        exResp = object [ "coordinates" .=
                          object [ "latitude" .= Number 33.749
                                 , "longitude" .= Number 84.388
                                 ]
                        ]
      in szTest "coordinates, both" lk exReq exResp
    ]

  , testGroup "Of Person"

    [ let
        lk = Ex.firstName
        exReq = object ["firstName" .= True]
        exResp = object ["firstName" .= String "Joseph"]
      in szTest "firstName" lk exReq exResp

    , let
        lk = Ex.lastName
        exReq = object ["lastName" .= True]
        exResp = object ["lastName" .= String "Abrahamson"]
      in szTest "lastName" lk exReq exResp

    , let
        lk = Ex.hometown $ do
          nm <- Ex.name
          crd <- Ex.coordinates $ do
            lat <- Ex.latitude
            lon <- Ex.longitude
            return (lat, lon)
          return (nm, crd)
        exReq =
          object [ "hometown" .=
                    object [ "name" .= True
                           , "coordinates" .= object [ "latitude" .= True
                                                     , "longitude" .= True
                                                     ]
                          ]
                  ]
        exResp =
          object [ "hometown" .=
                   object [ "the" .=
                            object [ "name" .= String "Atlanta"
                                   , "coordinates" .= object [ "latitude" .= Number 33.7490
                                                             , "longitude" .= Number 84.3880
                                                             ]
                                   ]
                          ]
                 ]
      in szTest "hometown name and coordinates" lk exReq exResp

    , let
        lk =
          Ex.hometown $ Ex.mayor $
          Ex.hometown $ Ex.mayor $
          Ex.hometown $ Ex.mayor $ pure ()
        htmy x = object ["hometown" .= object [ "mayor" .= x]]
        htmyResp x = object ["hometown" .= object ["the" .= object [ "mayor" .= object ["the" .= x]]]]
        exReq = htmy (htmy (htmy (object [])))
        exResp = htmyResp (htmyResp (htmyResp (object [])))
      in szTest "mayor loop" lk exReq exResp
    ]

  , testGroup "Of Api"

    [ let
        lk1 = subLookup (Ex.getCity "Atlanta") (pure ())
        lk2 = subLookup (Ex.getCity "Atlanta") Ex.name
        lk3 = subLookup (Ex.getCity "Fakeville") (pure ())
        lk4 = subLookup (Ex.getCity "DoesNotExist") (pure ())
        lk = (,,,) <$> lk1 <*> lk2 <*> lk3 <*> lk4
        exReq =
          object ["getCity" .=
                   -- NOTE: These have to be in Ord-order on args to get Value-equality
                   [ toJSON (String "Atlanta", object ["name" .= True])
                   , toJSON (String "DoesNotExist", object [])
                   , toJSON (String "Fakeville", object [])
                   ]
                 ]
        exResp =
          object ["getCity" .=
                   -- NOTE: These have to be in Ord-order on args to get Value-equality
                   [ toJSON ( String "Atlanta"
                            , object ["the" .= object ["name" .= String "Atlanta"]]
                            )
                   , toJSON ( String "DoesNotExist"
                            , Null
                            )
                   , toJSON ( String "Fakeville"
                            , object ["the" .= object []]
                            )
                   ]
                 ]
      in szTest "getCity" lk exReq exResp

    , let
        lk = Ex.getAllCities (withErr Ex.name)
        exReq =
          object ["getAllCities" .= object ["name" .= True]]
        exResp =
          object ["getAllCities" .=
                   object [ "ok" .= [ object ["name" .= String "Atlanta"]
                                    , object ["name" .= String "Fakeville"]
                                    ]
                          ]
                 ]
      in szTest "getAllCities (error ok)" lk exReq exResp

    , let
        lk = Ex.getCurrentUser (withErr Ex.firstName)
        exReq =
          object ["getCurrentUser" .= object ["firstName" .= True]]
        exResp =
          object ["getCurrentUser" .= object ["err" .= String "NotAuthorized"]]
      in szTest "getAllCities (error ok)" lk exReq exResp

    ]

  ]
