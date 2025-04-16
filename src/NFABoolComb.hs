{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : NFABoolComb
-- Description : Provides operations for combining NFAs using boolean operations
--
-- This module defines functions to manipulate and combine Non-deterministic
-- Finite Automata (NFAs) using boolean operations. In particular, it includes
-- functionality to compute the symmetric difference of two NFAs. The symmetric
-- difference operation constructs a new NFA that recognizes strings accepted
-- by exactly one of the input NFAs, but not both.
--
-- The primary function in this module is:
--
-- * 'symDiff': Computes the symmetric difference of two NFAs by determinizing
--   them and constructing their product automaton.
module NFABoolComb where

import Control.Monad (when)
import Control.Monad.State (evalState, get, modify)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (first3, second3, third3)
import NFA (NFA (NFA, final), getAlphabet, initial, reverseTransitionMap, sendsStateSet)

-- | Returns an NFA recognizing the symmetric difference of two NFAs, computing the product of the determinised automata
symDiff ::
  (Ord state1, Ord state2, Ord symbol) =>
  -- | The NFA A1
  NFA symbol state1 ->
  -- | The NFA A2
  NFA symbol state2 ->
  -- | The resulting NFA (A1 Δ A2)
  NFA symbol (Set state1, Set state2)
symDiff nfa1 nfa2 = NFA is f' delta' (reverseTransitionMap delta')
  where
    f1 = final nfa1
    f2 = final nfa2
    alpha = getAlphabet nfa1 `Set.union` getAlphabet nfa2
    is = Set.singleton (initial nfa1, initial nfa2)
    (f', delta', _) = evalState (aux is) (Set.empty, Map.empty, Set.empty)
    aux nexts = do
      if Set.null nexts
        then get
        else do
          nexts' <- Set.unions <$> traverse deal (Set.toList nexts)
          (_, _, visited) <- get
          aux $ Set.difference nexts' visited
    deal ps@(p1s, p2s) = do
      set_visited ps
      when (Set.disjoint p1s f1 /= Set.disjoint p2s f2) $ mark_fin ps
      let (a_to_s, nexts) = F.foldMap (\a -> let succs = Set.singleton (sendsStateSet nfa1 a p1s, sendsStateSet nfa2 a p2s) in (Map.singleton a succs, succs)) alpha
      modify $ second3 (<> Map.singleton ps a_to_s)
      return nexts
      where
        set_visited = modify . third3 . Set.insert
        mark_fin = modify . first3 . Set.insert
