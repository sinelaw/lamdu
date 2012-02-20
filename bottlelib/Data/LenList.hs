{-# LANGUAGE GADTs #-}
-- TODO: Just use HList with a class that implements a constraint that
-- it is homogenous after all? We just need the length encoded in the
-- type
module Data.LenList(
  LenList(..),
  head, tail, init, last,
  lengthNat, length
) where

import qualified TypeLevel.NaturalNumber as Nat
import Prelude hiding (head, tail, init, last, length)
import qualified Prelude as P

data LenList n a where
  Empty :: LenList Nat.Zero a
  Cons :: a -> LenList n a -> LenList (Nat.SuccessorTo n) a

data Fin n where
  FinZero :: Fin (Nat.SuccessorTo n)
  FinSucc :: Fin n -> Fin (Nat.SuccessorTo n)

lengthNat :: LenList n a -> n
lengthNat = undefined

length :: Nat.NaturalNumber n => LenList n a -> Int
length = Nat.naturalNumberAsInt . lengthNat

toList :: LenList n a -> [a]
toList Empty = []
toList (Cons x xs) = x : toList xs

instance Show a => Show (LenList n a) where
  show = show . toList

head :: LenList (Nat.SuccessorTo n) a -> a
head (Cons x _) = x

tail :: LenList (Nat.SuccessorTo n) a -> LenList n a
tail (Cons _ xs) = xs

last :: LenList (Nat.SuccessorTo n) a -> a
last (Cons x Empty) = x
last (Cons _ xs@(Cons _ _)) = last xs

init :: LenList (Nat.SuccessorTo n) a -> LenList n a
init (Cons _ Empty) = Empty
init (Cons x xs@(Cons _ _)) = Cons x (init xs)
