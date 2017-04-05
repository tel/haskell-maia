-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Iso to @Maybe@ with different semantic content.
module Maia.Internal.Provided where

import Control.Monad (ap)

data Provided a
  = Provided a
  | Absent
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Applicative Provided where
  pure = Provided
  (<*>) = ap

instance Monad Provided where
  return = pure
  Absent >>= _ = Absent
  Provided a >>= k = k a

runProvided :: Provided a -> Maybe a
runProvided Absent = Nothing
runProvided (Provided a) = Just a

bestProvided :: (a -> a -> a) -> Provided a -> Provided a -> Provided a
bestProvided _ Absent p = p
bestProvided _ p Absent = p
bestProvided f (Provided a) (Provided b) = Provided (f a b)
