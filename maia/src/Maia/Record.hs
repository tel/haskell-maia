-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Maia.Record where

import Data.Singletons
import GHC.TypeLits

--------------------------------------------------------------------------------

data Named k where
  (:-) :: n -> k -> Named k

type n :- v = n ':- v

infixr 6 :-

data instance Sing (s :: Named v) where
  SNamed :: (s ~ (n :- v), KnownSymbol n) => Sing n -> Sing v -> Sing (n :- v)

instance (KnownSymbol n, SingI n, SingI v) => SingI (n :- v) where
  sing = SNamed sing sing

--------------------------------------------------------------------------------

data Rec (f :: k -> *) (rs :: [Named k]) where
  RNil :: Rec f '[]
  (:&) :: f s -> Rec f rs -> Rec f (n :- s ': rs)

infixr 7 :&

instance Show (Rec f '[]) where
  show RNil = "RNil"

instance (Show (f s), Show (Rec f rs)) => Show (Rec f (n :- s ': rs)) where
  showsPrec p (h :& t) =
    if p >= 11 then showString "(" . core 0 . showString ")" else core p
    where core p' = showsPrec p' h . showString " :& " . showsPrec p' t
