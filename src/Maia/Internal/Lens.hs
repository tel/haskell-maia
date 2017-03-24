-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Tiny replica of a Lens library.
module Maia.Internal.Lens where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Vinyl

type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)
type Lens' s a = Lens s s a a

type RecSel rs ss = forall f . Lens' (Rec f rs) (Rec f ss)

lget :: Lens s t a b -> s -> a
lget l = getConst . l Const

lover :: Lens s t a b -> (a -> b) -> (s -> t)
lover l f = runIdentity . l (Identity . f)

lset :: Lens s t a b -> b -> s -> t
lset l = lover l . const

rlHead :: Lens (Rec f (r ': rs)) (Rec f (s ': rs)) (f r) (f s)
rlHead inj (hd :& tl) = (\hd' -> hd' :& tl) <$> inj hd

rlTail :: Lens (Rec f (r ': rs)) (Rec f (r ': ss)) (Rec f rs) (Rec f ss)
rlTail inj (hd :& tl) = (\tl' -> hd :& tl') <$> inj tl
