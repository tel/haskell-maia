-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Maia.Lookup where

import Data.Monoid
import Data.Singletons
import Maia.Language
import Maia.Lookup.Error
import Maia.Request
import Maia.Response

data Lookup t e r =
  Lookup { request :: Request t
         , responseHandler :: Response t -> Result e r
         }

instance Functor (Lookup t e) where
  fmap f (Lookup req resph ) =
    Lookup req (\resp -> fmap f (resph resp))

instance SingI (Fields t) => Applicative (Lookup t e) where

  pure a =
    Lookup mempty $ \_ -> pure a

  Lookup req1 resph1 <*> Lookup req2 resph2 =
    Lookup (req1 <> req2) $ \resp ->
      resph1 resp <*> resph2 resp where

newtype Result e a =
  Result { runResult :: Either (LookupError e) a }
  deriving (Show, Eq, Functor)

instance Applicative (Result e) where
  pure = Result . pure
  Result (Right f) <*> Result (Right a) = Result (Right (f a))
  Result (Left e1) <*> Result (Left e2) = Result (Left (e1 `Branch` e2))
  (Result (Left e)) <*> _ = Result (Left e)
  _ <*> (Result (Left e)) = Result (Left e)

nestResult :: String -> Result e a -> Result e a
nestResult _ r@(Result (Right _)) = r
nestResult n (Result (Left e)) = Result (Left (Nest n e))
