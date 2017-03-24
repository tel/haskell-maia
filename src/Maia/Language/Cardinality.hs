-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Maia.Language.Cardinality where

import Data.Singletons

data Cardinality where
  One :: Cardinality
  Opt :: Cardinality
  Many :: Cardinality

type One = 'One
type Opt = 'Opt
type Many = 'Many

data instance Sing (c :: Cardinality) where
  SOne :: c ~ One => Sing c
  SOpt :: c ~ Opt => Sing c
  SMany :: c ~ Many => Sing c

instance SingI One where sing = SOne
instance SingI Opt where sing = SOpt
instance SingI Many where sing = SMany

type family CollectionOf (c :: Cardinality) a where
  CollectionOf One a = a
  CollectionOf Opt a = Maybe a
  CollectionOf Many a = [a]

traverseColl ::
  Applicative f => Sing c -> (a -> f b) ->
  CollectionOf c a -> f (CollectionOf c b)
traverseColl SOne inj a = inj a
traverseColl SOpt inj ma = traverse inj ma
traverseColl SMany inj ma = traverse inj ma
