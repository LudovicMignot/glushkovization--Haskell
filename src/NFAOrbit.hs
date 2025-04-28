{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : NFAOrbit
-- Description : Provides functions for computing orbits, isolating states, and stabilizing NFAs
--
-- This module defines a set of functions and utilities for working with
-- Non-deterministic Finite Automata (NFAs). It includes algorithms for:
--
-- * Computing the orbits of an NFA using Kosaraju's algorithm.
-- * Identifying and isolating ingates and outgates of orbits.
-- * Substituting orbits with orbital automata.
-- * Stabilizing NFAs and testing their stability.
module NFAOrbit where

import Control.Monad (unless)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Free (Free (Free, Pure))
import Control.Monad.State.Lazy
  ( MonadState (get),
    evalState,
    execState,
    modify,
  )
import Data.Bifunctor (first, second)
import qualified Data.Foldable as F
import Data.Function ((&))
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set (delete, difference, disjoint, empty, filter, findMax, foldl', fromList, insert, intersection, isSubsetOf, lookupMax, map, member, null, singleton, toList, union)
import MonoEither (FreeEither, MonoEither (MonoEither))
import NFA (NFA (NFA, delta_rev, final), addSuccsInMap, addTransitionInMap, delta, filterTransformsSuccs, foldMapSuccs, getPreds, getStates, getSuccs, getSuccsInTransitionMap, getSuccsWithSymbol, initial, isFinal, isInitial, mapState, removeStates, restrictSuccs, reverse, reverseTransitionMap, transformsSuccs)
import NFAAccessibility (getAccessibleStatesFromVia)
import NFAHomogeneity (getIncomingSymbol)

-- * Computation of the orbits

-- | Computes the first step of the Kosaraju's Algorithm (post order depth-first search)
kosaraju1 :: (Ord state) => NFA symbol state -> [state]
kosaraju1 nfa = snd $ execState (mapM_ aux $ Set.toList $ getStates nfa) (Set.empty, [])
  where
    aux p = do
      (marked, _) <- get
      unless (Set.member p marked) $
        do
          mark p
          mapM_ aux $ Set.toList $ getSuccs nfa p
          prepend p
    mark = modify . first . Set.insert
    prepend = modify . second . (:)

-- | Computes the second step of the Kosaraju's Algorithm (search in the reversal following the list obtained from step 1)
kosaraju2 :: (Ord state) => NFA symbol state -> [state] -> [[state]]
kosaraju2 nfa kos1List = filter (not . null) $ evalState (traverse aux kos1List) Set.empty
  where
    aux p = do
      marked <- get
      if Set.member p marked
        then
          return []
        else do
          mark p
          fmap (p :) $ concatMapM aux $ Set.toList $ getSuccs rev_nfa p
    mark = modify . Set.insert
    rev_nfa = NFA.reverse nfa

-- | Computes the orbits of an NFA following the Kosaraju's Algorithm
kosaraju ::
  (Ord state) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The orbits of A
  [[state]]
kosaraju nfa = kosaraju2 nfa $ kosaraju1 nfa

-- | Computes the orbits of an NFA following the Kosaraju's Algorithm, as a set of set of states
kosarajuSet ::
  (Ord state) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The orbits of A
  Set (Set state)
kosarajuSet nfa = Set.fromList $ Set.fromList <$> kosaraju nfa

-- * Computation of the outgates

-- | For a given set O of states, returns the states of O that are final or linked with a state not in O.
-- If O is an orbit, returns its outgates
outgates ::
  (Ord state) =>
  -- | The NFA A
  NFA symbol state ->
  -- | A (possibly) orbit O of A
  Set state ->
  -- | The set of the (possibly) outgates of O
  Set state
outgates nfa orbit = Set.filter (\p -> isFinal nfa p || F.any (not . (`Set.member` orbit)) (getSuccs nfa p)) orbit

-- | For a given set O of states, returns the states of O that are initial or preceded by a state not in O.
ingates :: (Ord t) => NFA symbol t -> Set t -> Set t
ingates nfa orbit = Set.filter (\p -> isInitial nfa p || F.any (not . (`Set.member` orbit)) (getPreds nfa p)) orbit

-- * Isolation functions

-- | For a given set O of states and a given state g, adds a clone of O where g is the only state which is final or linked with the outside of O.
-- The original g is not final nor linked with the "outside" of O anymore.
-- If O is an orbit and g a state of O, performs the outgate isolation of O.
-- The new added state "Right" ones, the "old" ones "Left" ones.
outgateIsolation ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | A (possibly) orbit O of A
  Set state ->
  -- | A state g
  state ->
  -- | The (possible) outgate isolation of g
  NFA symbol (Either state state)
outgateIsolation nfa orbit g = NFA initial' final' delta' (reverseTransitionMap delta')
  where
    toRight = either Right Right
    orbit_l = Set.map Left orbit
    others_l = Set.map Left $ getStates nfa `Set.difference` orbit
    nfa' = mapState Left nfa
    initial' = Set.union (initial nfa') $ Set.map Right $ Set.intersection orbit $ initial nfa
    final'
      | isFinal nfa g = Set.insert (Right g) $ Set.delete (Left g) $ final nfa'
      | otherwise = final nfa'
    delta' =
      delta nfa'
        -- successors of "old" g are restricted to the "old" orbit
        & restrictSuccs (Left g) orbit_l
        -- successors of "new" g are the "new" ones in the clone, and the "old" external ones
        & Map.insert (Right g) (maybe Map.empty (transformsSuccs (\s -> if Set.member s orbit then Right s else Left s)) (getSuccsInTransitionMap (delta nfa) g))
        -- successors of "new" o' are restricted to the "new" orbit, successors of "old" o are not modified
        & flip
          ( Set.foldl'
              ( \trans o ->
                  Map.insert
                    (Right o)
                    ( maybe
                        Map.empty
                        (filterTransformsSuccs Right (`Set.member` orbit))
                        (getSuccsInTransitionMap (delta nfa) o)
                    )
                    trans
              )
          )
          (Set.delete g orbit)
        -- for the other "old" states, add a link to o' if o is a successor
        & flip
          ( Set.foldl'
              ( flip
                  ( Map.adjust
                      (foldMapSuccs (\p -> if Set.member p orbit_l then Set.fromList [p, toRight p] else Set.singleton p))
                  )
              )
          )
          others_l

-- | For a given set O of states and a given state g, adds a clone of O \ {g}.
-- The original O has only one incoming link with the outside through g.
-- If O is an orbit and g a state of O, performs the ingate isolation of O.
-- The new added state "Right" ones, the "old" ones "Left" ones.
-- Also returns the new added states
ingateIsolation ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | A (possibly) orbit O of A
  Set state ->
  -- | A state g
  state ->
  -- | The (possible) ingate isolation of g
  (NFA symbol (Either state state), Set (Either state state))
ingateIsolation nfa orbit g =
  -- ici remplacer trim
  -- trim $ NFA initial' final' delta' (reverseTransitionMap delta')
  (NFA initial' final' delta' (reverseTransitionMap delta'), Set.map Right o_no_g_acc)
  where
    nfa' = mapState Left nfa
    toRight = either Right Right
    o_no_g = Set.delete g orbit
    o_no_g_l = Set.map Left o_no_g
    o_no_g_acc = getAccessibleStatesFromVia nfa (Set.delete g $ ingates nfa orbit) o_no_g
    others = getStates nfa `Set.difference` orbit
    others_l = Set.map Left others
    initial' = Set.union (initial nfa' `Set.difference` o_no_g_l) $ Set.map Right $ initial nfa `Set.intersection` o_no_g_acc
    final' = Set.union (final nfa') $ Set.map Right $ final nfa `Set.intersection` o_no_g_acc
    delta' =
      delta nfa'
        -- The "new" o' are not linked anymore to the "old" O except for g,
        -- are linked to the "new" o' if they were linked to an o,
        -- and to the other "old" states outside of O
        & flip
          ( Set.foldl'
              ( \trans o ->
                  Map.insert
                    (Right o)
                    ( maybe
                        Map.empty
                        (transformsSuccs (\p -> if Set.member p o_no_g then Right p else Left p))
                        (getSuccsInTransitionMap (delta nfa) o)
                    )
                    trans
              )
          )
          -- ici modif o_no_g
          o_no_g_acc
        -- The "old" other states outside of O are linked to the "new" o' if they were linked to an old "o"
        & flip
          ( Set.foldl'
              ( flip
                  ( Map.adjust
                      (transformsSuccs (\p -> if Set.member p o_no_g_l then toRight p else p))
                  )
              )
          )
          others_l

-- * Orbital isolation

-- | Tests whether an orbit is outgate isolated
isExtIsolated :: (Ord state) => NFA symbol state -> Set state -> Bool
isExtIsolated nfa orbit = F.length (outgates nfa orbit) <= 1

-- | Tests whether an NFA is outgate isolated
isNFAExtIsolated :: (Ord state) => NFA symbol state -> Bool
isNFAExtIsolated nfa = F.all (isExtIsolated nfa) $ kosarajuSet nfa

-- | Tests whether an orbit is ingate isolated
isIntIsolated :: (Ord state) => NFA symbol state -> Set state -> Bool
isIntIsolated nfa orbit = F.length (ingates nfa orbit) <= 1

-- | Determines whether the orbits of an NFA are isolated
isIsolatedNFA :: (Ord state) => NFA symbol state -> Bool
isIsolatedNFA nfa = F.all (\o -> isExtIsolated nfa o && isIntIsolated nfa o) $ kosarajuSet nfa

-- | Isolates the orbits of an NFA, using a successor of an outgate to become an ingate.
-- Removes orbits with no outgates or no ingates.
-- May create new orbits, that will be isolate too
orbitalIsolationViaSuccOutgates ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The resulting automaton
  NFA symbol (FreeEither state)
orbitalIsolationViaSuccOutgates nfa = aux nfa' orbits
  where
    nfa' = mapState Pure nfa
    orbits = Set.fromList <$> kosaraju nfa'
    aux aut [] = aut
    aux aut orbs@(o : os)
      | Set.null outs || Set.null ins = aux (removeStates aut o) os
      | F.length o == 1 = aux aut os
      | isExtIso && (isIntIso && not (F.foldMap (getSuccs aut) outs `Set.disjoint` ins)) = aux aut os
      | not isExtIso = aux (mapState (Free . MonoEither) $ outgateIsolation aut o g) (Set.map (Free . MonoEither . Right) o : (Set.map (Free . MonoEither . Left) <$> orbs))
      | otherwise = aux aut' ((Set.map (Free . MonoEither) <$> new_orbs) <> (Set.map (Free . MonoEither . Left) <$> os))
      where
        outs = outgates aut o
        ins = ingates aut o
        isExtIso = isExtIsolated aut o
        isIntIso = isIntIsolated aut o
        g = Set.findMax outs
        succ_out = Set.findMax $ getSuccs aut g `Set.intersection` o
        (aut'_, new_states) = ingateIsolation aut o succ_out
        aut' = mapState (Free . MonoEither) aut'_
        new_orbs = fmap Set.fromList $ kosaraju $ removeStates aut'_ $ getStates aut'_ `Set.difference` new_states -- (Set.delete succ_out o)

-- * Orbital substitution functions

-- | Substitutes an orbit O of a standard NFA A by a standard orbital automaton A'
-- The new added state "Right" ones, the "old" ones "Left" ones.
orbitalSubstitution ::
  (Ord state, Ord state', Ord symbol) =>
  -- | the NFA A
  NFA symbol state ->
  -- | A (possibly) isolated orbit O of A
  Set state ->
  -- | A standard automaton A'
  NFA symbol state' ->
  -- | The resulting automaton
  NFA symbol (Either state state')
orbitalSubstitution nfa o nfa' = removeStates (NFA initial'' final'' delta'' (reverseTransitionMap delta'')) o_l
  where
    nfa_l = mapState Left nfa
    first_nfa' = F.foldMap (getSuccs nfa') $ initial nfa'
    out_o = outgates nfa o
    nfa_r = mapState Right $ removeStates nfa' $ initial nfa'
    others_l = Set.map Left $ getStates nfa `Set.difference` o
    o_l = Set.map Left o
    succs_out_o = F.foldMap (getSuccsWithSymbol nfa) out_o
    final' = final nfa'
    initial'' = initial nfa_l
    final''
      | final nfa `Set.disjoint` o = final nfa_l
      | otherwise = final nfa_l `Set.union` Set.map Right (final nfa')
    delta'' =
      -- the new transitions are made of the unions of the transitions of the two automata A and A'
      delta nfa_l `Map.union` delta nfa_r
        -- for any predecessor of the ingates of O, we add the successors of the initial state of A'
        & flip
          ( Set.foldl'
              ( flip
                  ( Map.adjust
                      (foldMapSuccs (\p -> if Set.member p o_l then Set.map Right first_nfa' else Set.singleton p))
                  )
              )
          )
          others_l
        -- for any final state of A', we add the successors of the outgates of O
        & flip
          (Set.foldl' (flip (\fin' -> flip (Set.foldl' (flip (\(state, symbol) -> flip addTransitionInMap (Right fin', symbol, Left state)))) succs_out_o)))
          final'

-- * Stabilization functions

-- | Returns the orbital NFA associated with an isolated orbit in an homogeneous NFA
orbitalNFA :: (Ord state, Ord symbol) => NFA symbol state -> Set state -> NFA symbol (Maybe state)
orbitalNFA aut o = NFA (Set.singleton Nothing) (Set.map Just outs) (delta aut2') (delta_rev aut2')
  where
    ins = ingates aut o
    outs = outgates aut o
    -- 1 (first_symb, _) = head $ Map.toList $ fromJust $ Map.lookup (fromJust $ Set.lookupMax ins) $ delta_rev aut
    -- 2 tmp = Map.toList <$> ((Set.lookupMax ins) >>= \r -> Map.lookup r $ delta_rev aut)
    -- 2 aut' = case tmp of
    -- 2  Just ((first_symb, _) : _) -> addTransitions (mapState Just $ removeStates aut $ getStates aut `Set.difference` o) first_symb (Set.singleton Nothing) (Set.map Just ins)
    -- 2   _ -> mapState Just $ removeStates aut $ getStates aut `Set.difference` o
    aut2 = mapState Just $ removeStates aut $ getStates aut `Set.difference` o
    aut2' = Set.foldl' (\g res -> addTransitions res (fromJust $ getIncomingSymbol aut g) (Set.singleton Nothing) (Set.singleton $ Just g)) aut2 ins

-- | Removes transitions from a set of states to another set of states by a given symbol
removeTransFromTo :: (Ord state, Ord symbol) => NFA symbol state -> symbol -> Set state -> Set state -> NFA symbol state
removeTransFromTo aut a from to = NFA (initial aut) (final aut) delta' (reverseTransitionMap delta')
  where
    delta' = Set.foldl' (flip (Map.adjust (Map.adjust (`Set.difference` to) a))) (delta aut) from

-- | Adds transitions to an NFA
addTransitions :: (Ord symbol, Ord state) => NFA symbol state -> symbol -> Set state -> Set state -> NFA symbol state
addTransitions aut a from to = NFA (initial aut) (final aut) delta' (reverseTransitionMap delta')
  where
    delta' = Set.foldl' (\trans s -> addSuccsInMap trans s a to) (delta aut) from

-- | Strongly stabilizes an isolated orbit of an NFA
stabilizeOrbit :: (Ord state, Ord symbol) => NFA symbol state -> Set state -> NFA symbol (Either state (FreeEither (Maybe state)))
stabilizeOrbit aut o = orbitalSubstitution aut o aut_o_stab_res
  where
    aut_o = orbitalNFA aut o
    (first_symb, _) = head $ Map.toList (fromJust $ Map.lookup (fromJust $ Set.lookupMax $ initial aut_o) $ delta aut_o)
    aut_o' =
      removeTransFromTo aut_o first_symb (final aut_o) (F.foldMap (getSuccs aut_o) $ initial aut_o)
    aut_o_stab =
      strongStabilizationNFA aut_o'
    aut_o_stab_res = addTransitions aut_o_stab first_symb (final aut_o_stab) (F.foldMap (getSuccs aut_o_stab) $ initial aut_o_stab)

-- | Strongly stabilizes an homogeneous and standard NFA
strongStabilizationNFA :: (Ord state, Ord symbol) => NFA symbol state -> NFA symbol (FreeEither state)
strongStabilizationNFA nfa = aux nfa' orbits
  where
    nfa' = orbitalIsolationViaSuccOutgates nfa
    orbits = Set.fromList <$> kosaraju nfa'
    aux aut [] = aut
    aux aut (o : os)
      | F.length o == 1 = aux aut os
      | Set.null (ingates aut o) || Set.null (outgates aut o) = aux aut os
    aux aut (o : os) = aux aut' $ Set.map (Free . MonoEither . Left) <$> os
      where
        aut_o = orbitalNFA aut o
        (first_symb, _) = head $ Map.toList (fromJust $ Map.lookup (fromJust $ Set.lookupMax $ initial aut_o) $ delta aut_o)
        aut_o' =
          removeTransFromTo aut_o first_symb (final aut_o) (F.foldMap (getSuccs aut_o) $ initial aut_o)
        aut_o_stab =
          strongStabilizationNFA aut_o'
        aut_o_stab_res = addTransitions aut_o_stab first_symb (final aut_o_stab) (F.foldMap (getSuccs aut_o_stab) $ initial aut_o_stab)
        aut' =
          mapState
            ( \case
                Left s -> Free $ MonoEither $ Left s
                Right s -> s >>= fromJust
            )
            $ orbitalSubstitution aut o aut_o_stab_res

-- | Tests whether an orbit is externally stable
isExtStable :: (Ord state) => NFA symbol state -> Set state -> Bool
isExtStable aut o =
  allEqual preds_out_o && allEqual succs_out_o
  where
    ins = Set.toList $ ingates aut o
    outs = Set.toList $ outgates aut o
    preds_out_o = (\s -> getPreds aut s `Set.difference` o) <$> ins
    succs_out_o = (\s -> getSuccs aut s `Set.difference` o) <$> outs
    allEqual [] = True
    allEqual (x : xs) = all (== x) xs

-- | Tests whether an orbit is internally stable
isIntStable :: (Ord state) => NFA symbol state -> Set state -> Bool
isIntStable aut o = F.length o <= 1 || all (\g -> ins `Set.isSubsetOf` getSuccs aut g) outs
  where
    ins = ingates aut o
    outs = outgates aut o

-- | Tests whether an orbit is stable
isStable :: (Ord state) => NFA symbol state -> Set state -> Bool
isStable aut o = isExtStable aut o && isIntStable aut o

-- | Tests whether an orbit is strongly stable
isStronglyStable :: (Ord state, Ord symbol, Show state, Show symbol) => NFA symbol state -> Set state -> Bool
isStronglyStable aut o = F.length o <= 1 || isStable aut o && all (isStronglyStable aut_o') orbits
  where
    aut_o = orbitalNFA aut o
    (first_symb, _) = head $ Map.toList (fromJust $ Map.lookup (fromJust $ Set.lookupMax $ initial aut_o) $ delta aut_o)
    aut_o' = removeTransFromTo aut_o first_symb (final aut_o) (F.foldMap (getSuccs aut_o) $ initial aut_o)
    orbits = Set.fromList <$> kosaraju aut_o'

-- | Tests whether an NFA is strongly stable
isStronglyStableNFA :: (Ord state, Ord symbol, Show state, Show symbol) => NFA symbol state -> Bool
isStronglyStableNFA aut = all (isStronglyStable aut) $ kosarajuSet aut

-- | Tests whether an NFA is stable
isStableNFA :: (Ord state) => NFA symbol state -> Bool
isStableNFA aut = all (isStable aut) $ kosarajuSet aut