-- |
-- Module      : NFAStandard
-- Description : Provides functions to determine if a Non-deterministic Finite Automaton (NFA) is standard and to convert an NFA into an equivalent standard NFA.
--
-- This module defines utilities for working with standard NFAs. A standard NFA is defined as an NFA with a single initial state that is not the destination of any transition. The module includes:
--
-- * A function to check if an NFA is standard.
-- * A function to transform a given NFA into an equivalent standard NFA by adding a new initial state.
-- * A generator for creating random standard NFAs.
--
-- The module relies on the `NFA` module for the core NFA data structure and operations, and uses various utilities from the `Data.Map`, `Data.Set`, and `Data.Foldable` libraries for handling collections and transitions.
--
-- Key functions:
-- - `isStandard`: Checks if an NFA is standard.
-- - `makeStandard`: Converts an NFA into an equivalent standard NFA.
-- - `generateStandardNFA`: Generates a random standard NFA using specified parameters.
module NFAStandard where

import qualified Data.Foldable as F (all)
import qualified Data.Map as Map (empty, insert, lookup, singleton, unionWith)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set (disjoint, foldl', insert, singleton, size, union)
import NFA (NFA (NFA, delta_rev, initial), delta, final, generateNFASuchThat, mapState, reverseTransitionMap)

-- * Standard NFA

-- | Determines whether an NFA is standard, i.e. if there is only one initial state which is not the destination of a transition
isStandard ::
  (Ord symbol) =>
  -- | The NFA A
  NFA state symbol ->
  -- | The Boolean "A is standard"
  Bool
isStandard nfa = Set.size (initial nfa) == 1 && F.all (F.all $ Set.disjoint $ initial nfa) (delta nfa)

-- Computes an equivalent standard NFA from an NFA by adding a initial state
makeStandard ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The standard NFA equivalent to A
  NFA symbol (Maybe state)
makeStandard nfa =
  NFA
    { initial = Set.singleton Nothing,
      final = final',
      delta = Map.insert Nothing succ_inits delta_just,
      delta_rev = Map.unionWith (Map.unionWith Set.union) inv_succ_init_map (delta_rev nfa_just)
    }
  where
    -- equivalent to nfa, where the states p are "promoted" as Just p
    nfa_just = NFA.mapState Just nfa
    -- equivalent to delta, where the states p are "promoted" as Just p
    delta_just = delta nfa_just
    -- the successors of the old initial states
    succ_inits = Set.foldl' (\res i -> Maybe.maybe res (Map.unionWith Set.union res) $ Map.lookup i delta_just) Map.empty $ initial nfa_just
    -- the final states p "promoted" as Just p
    just_final = final nfa_just
    -- The new final states, adding the initial states if at least one old initial state was final
    final'
      | Set.disjoint (initial nfa) (final nfa) = just_final
      | otherwise = Set.insert Nothing just_final

    inv_succ_init_map = reverseTransitionMap $ Map.singleton Nothing succ_inits

-- | Generates an homogeneous NFA using the corresponding makeGenNFA generator
generateStandardNFA :: (Ord state, Ord symbol) => [symbol] -> [state] -> Int -> Int -> Int -> IO (NFA symbol state)
generateStandardNFA = generateNFASuchThat isStandard