{-# LANGUAGE
  DeriveFunctor, KindSignatures,
  FlexibleInstances, FlexibleContexts,
  OverlappingInstances #-}
-- TODO: Just use HList with a class that implements a constraint that
-- it is homogenous after all? We just need the length encoded in the
-- type
module Data.LenList(ZeroL(..), SuccL(..), (.*.), nil) where

import Prelude ((.), Show(..), Read, Eq, Ord, Functor)
import qualified Prelude as P
import Data.Monoid(mconcat)

data ZeroL a = ZeroL
  deriving (Show, Read, Eq, Ord, Functor)

data SuccL l a = SuccL {
  succHead :: a,
  succTail :: (l a)
} deriving (Read, Eq, Ord, Functor)

instance (Show (l a), Show a) => Show (SuccL l a) where
  show (SuccL h t) =
    mconcat ["SuccL (", show h, ") (", show t, ")"]

class LenList (l :: * -> *)
instance LenList ZeroL
instance LenList l => LenList (SuccL l)

infixr 5 .*.
(.*.) :: (LenList l1) => a -> l1 a -> (SuccL l1) a
(.*.) = SuccL

nil :: ZeroL a
nil = ZeroL

class Head l where
  head :: l a -> a
instance Head (SuccL l) where
  head = succHead

class Tail l where
  tail :: SuccL l a -> l a
instance Tail (SuccL l) where
  tail = succTail

class Last l where
  last :: l a -> a
instance Last (SuccL ZeroL) where
  last = succHead
instance Last l => Last (SuccL l) where
  last = last . succTail

class ToList l where
  toList :: l a -> [a]

instance ToList ZeroL where
  toList ZeroL = []

instance ToList l => ToList (SuccL l) where
  toList (SuccL h t) = h : toList t
