{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- | Module      : MonoEither
-- | Description : Provides a newtype wrapper for 'Either' with identical types on both sides,
-- |               along with instances for common typeclasses and utility functions.
-- |
-- | This module defines the 'MonoEither' type, which is a wrapper around 'Either' where both
-- | the 'Left' and 'Right' constructors contain values of the same type. It provides instances
-- | for 'Show1', 'Eq1', 'Ord1', and 'Functor', enabling advanced functionality for working
-- | with this type. Additionally, it defines a type alias 'FreeEither' for the free monad
-- | over 'MonoEither', and utility functions for converting between 'Either' and 'FreeEither'.
-- |
-- | The main use case for 'MonoEither' is when you need a variant of 'Either' where both
-- | branches share the same type, and you want to leverage typeclass instances or work
-- | with free monads.
module MonoEither where

import Control.Monad.Free (Free (Free, Pure))
import Data.Functor.Classes (Eq1 (..), Ord1 (liftCompare), Show1 (liftShowsPrec))

-- | A type alias for the free monad over 'MonoEither'.
-- | This represents computations that can branch into 'Left' or 'Right' at each step,
-- | with both branches containing the same type.
newtype MonoEither a = MonoEither (Either a a)
  deriving (Eq, Ord)
  deriving newtype (Show)

instance Show1 MonoEither where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> MonoEither a -> ShowS
  liftShowsPrec f _ p (MonoEither (Left a)) = showParen (p > 10) $ showString "Left " . f 11 a
  liftShowsPrec f _ p (MonoEither (Right a)) = showParen (p > 10) $ showString "Right " . f 11 a

instance Eq1 MonoEither where
  liftEq :: (a -> b -> Bool) -> MonoEither a -> MonoEither b -> Bool
  liftEq f (MonoEither (Left a)) (MonoEither (Left b)) = f a b
  liftEq _ (MonoEither (Left _)) (MonoEither (Right _)) = False
  liftEq _ (MonoEither (Right _)) (MonoEither (Left _)) = False
  liftEq f (MonoEither (Right a)) (MonoEither (Right b)) = f a b

instance Ord1 MonoEither where
  liftCompare :: (a -> b -> Ordering) -> MonoEither a -> MonoEither b -> Ordering
  liftCompare f (MonoEither (Left a)) (MonoEither (Left b)) = f a b
  liftCompare _ (MonoEither (Left _)) (MonoEither (Right _)) = LT
  liftCompare _ (MonoEither (Right _)) (MonoEither (Left _)) = GT
  liftCompare f (MonoEither (Right a)) (MonoEither (Right b)) = f a b

instance Functor MonoEither where
  fmap :: (a -> b) -> MonoEither a -> MonoEither b
  fmap f (MonoEither (Left a)) = MonoEither $ Left $ f a
  fmap f (MonoEither (Right a)) = MonoEither $ Right $ f a

-- | A newtype wrapper around 'Either' where both 'Left' and 'Right' contain values of the same type.
-- | This type is useful for scenarios where you want to treat 'Either' symmetrically or need
-- | specialized typeclass instances.
type FreeEither a = Free MonoEither a

-- | Converts a standard 'Either' value into a 'FreeEither' value.
-- | This wraps the 'Either' value in the free monad structure.
-- |
-- | ==== __Examples__
-- |
-- | >>> eitherToFree (Left 42)
-- Free (Left (Pure 42))
-- |
-- | >>> eitherToFree (Right 42)
-- Free (Right (Pure 42))
eitherToFree :: Either a a -> FreeEither a
eitherToFree e = Free $ Pure <$> MonoEither e

-- | Extracts the final value from a 'FreeEither' computation.
-- | This function recursively evaluates the free monad structure until it reaches a 'Pure' value.
-- |
-- | ==== __Examples__
-- |
-- | >>> freeToA (Pure 42)
-- 42
-- |
-- | >>> freeToA (Free (MonoEither (Left (Pure 42))))
-- 42
-- |
-- | >>> freeToA (Free (MonoEither (Right (Pure 42))))
-- 42
freeToA :: FreeEither a -> a
freeToA (Pure a) = a
freeToA (Free (MonoEither (Left a))) = freeToA a
freeToA (Free (MonoEither (Right a))) = freeToA a
