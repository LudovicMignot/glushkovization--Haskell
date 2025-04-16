-- |
-- Module      : NFAComplete
-- Description : Provides functionality to complete a Non-deterministic Finite Automaton (NFA).
--
-- This module defines a function to transform an NFA into its complete form.
-- A complete NFA ensures that for every state and every symbol in the alphabet,
-- there is a defined transition. This is achieved by introducing a "sink" state
-- (represented as 'Nothing') to handle undefined transitions.
--
-- The module depends on the 'NFA' module, which provides the core data structure
-- and utility functions for working with NFAs. It also uses standard libraries
-- like 'Data.Foldable', 'Data.Map', and 'Data.Set' for functional and data manipulation.
module NFAComplete where

import qualified Data.Foldable as F
import qualified Data.Map as Map (singleton)
import qualified Data.Set as Set (map, null, singleton)
import NFA
  ( NFA (NFA, final, initial),
    getAlphabet,
    getStates,
    reverseTransitionMap,
    sendsState,
  )

-- | Completes an NFA
complete ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The complete NFA associated with A
  NFA symbol (Maybe state)
complete nfa = NFA (Set.map Just $ initial nfa) (Set.map Just $ final nfa) delta' (reverseTransitionMap delta')
  where
    alpha = getAlphabet nfa
    nothing_if_empty e = if Set.null e then Set.singleton Nothing else Set.map Just e
    delta' =
      Map.singleton Nothing (F.foldMap (\a -> Map.singleton a $ Set.singleton Nothing) alpha)
        <> F.foldMap (\p -> Map.singleton (Just p) $ F.foldMap (\a -> Map.singleton a (nothing_if_empty $ sendsState nfa a p)) alpha) (getStates nfa)
