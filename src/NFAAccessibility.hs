{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : NFAAccessibility
-- Description : Provides functions to analyze and manipulate the accessibility and coaccessibility of states in a Non-deterministic Finite Automaton (NFA).
--
-- This module defines utility functions for working with NFAs, including:
--
-- * Computing accessible states, coaccessible states, and useful states.
-- * Determining whether an NFA is trim (i.e., all states are both accessible and coaccessible).
-- * Trimming an NFA to keep only its useful states.
-- * Exploring accessibility from a restricted set of states.
--
-- The functions leverage both pure and monadic approaches for state exploration, and they rely on the `Set` data structure for efficient state management.
module NFAAccessibility where

import Control.Monad.State.Lazy
  ( MonadState (get),
    evalState,
    modify,
  )
import qualified Data.Foldable as F (foldMap)
import Data.Set (Set)
import qualified Data.Set as Set (difference, intersection, null, union)
import NFA (NFA, getStates, getSuccs, initial, removeStates, reverse)

-- | Returns the set of the accessible states
getAccessibleStates ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The accessible states
  Set state
getAccessibleStates nfa = aux (Set.difference succs_of_is is) is
  where
    is = initial nfa
    succs_of_is = F.foldMap (getSuccs nfa) is
    aux nexts access
      | Set.null nexts = access
      | otherwise = aux (Set.difference succs_of_nexts access') access'
      where
        access' = Set.union nexts access
        succs_of_nexts = F.foldMap (getSuccs nfa) nexts

-- | Equivalent to getAccessibleStates, but using the State monad
getAccessibleStates' ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The accessible states
  Set state
getAccessibleStates' nfa = evalState (aux $ Set.difference succs_of_is is) is
  where
    is = initial nfa
    succs_of_is = F.foldMap (getSuccs nfa) is
    aux nexts
      | Set.null nexts = get
      | otherwise = do
          modify (Set.union nexts)
          get >>= aux . Set.difference succs_of_nexts
      where
        succs_of_nexts = F.foldMap (getSuccs nfa) nexts

-- | Returns the set of the coaccessible states
getCoaccessibleStates ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The coaccessible states
  Set state
getCoaccessibleStates = getAccessibleStates' . NFA.reverse

-- | Returns the set of the useful states
getUsefulStates ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The useful states
  Set state
getUsefulStates nfa = getAccessibleStates' nfa `Set.intersection` getCoaccessibleStates nfa

-- | Returns the set of the accessible states from a set of states restricting the transitions defined by a
-- set of states
getAccessibleStatesFromVia ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The set of "initial" states
  Set state ->
  -- | The restriction of the considered states
  Set state ->
  -- | The accessible states
  Set state
getAccessibleStatesFromVia nfa is vs = evalState (aux $ Set.difference succs_of_is is) is
  where
    succs_of_is = F.foldMap (getSuccs nfa) is `Set.intersection` vs
    aux nexts
      | Set.null nexts = get
      | otherwise = do
          modify (Set.union nexts)
          get >>= aux . Set.difference succs_of_nexts
      where
        succs_of_nexts = F.foldMap (getSuccs nfa) nexts `Set.intersection` vs

-- * Trim operation

-- | Tests whether an NFA is trim
isTrim ::
  (Ord state) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The Boolean "A is trim"
  Bool
isTrim nfa = getStates nfa == getUsefulStates nfa

-- | Only keeps the useful states of an NFA
trim ::
  (Ord state) =>
  NFA symbol state ->
  -- | The resulting trim NFA
  NFA symbol state
trim nfa = removeStates nfa $ getStates nfa `Set.difference` getUsefulStates nfa