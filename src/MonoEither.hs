{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module MonoEither where

import Control.Monad.Free (Free (Free, Pure))
import Data.Functor.Classes (Eq1 (..), Ord1 (liftCompare), Show1 (liftShowsPrec))

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

type FreeEither a = Free MonoEither a

eitherToFree :: Either a a -> FreeEither a
eitherToFree e = Free $ Pure <$> MonoEither e

freeToA :: FreeEither a -> a
freeToA (Pure a) = a
freeToA (Free (MonoEither (Left a))) = freeToA a
freeToA (Free (MonoEither (Right a))) = freeToA a