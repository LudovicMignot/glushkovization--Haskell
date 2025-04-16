{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : NFA
-- Description : A module for representing and manipulating Non-deterministic Finite Automata (NFA).
--
-- This module provides a data type for representing Non-deterministic Finite Automata (NFA) and a variety of functions for manipulating and querying them.
-- It includes functionality for modifying NFAs, generating random NFAs, mapping over states, querying properties, simulating the automaton, and reversing transitions.
--
-- == The NFA type
-- The core type of this module is 'NFA', which represents a Non-deterministic Finite Automaton. It is parameterized over the type of states and the type of symbols in the alphabet.
--
-- == Modification functions
-- Functions for modifying the structure of an NFA, such as switching initial or final states, adding or removing transitions, and renumbering states.
--
-- == Arbitrary Instance
-- Functions for generating random NFAs for testing purposes, with or without constraints.
--
-- == Functorial fmap-like
-- A function for mapping over the states of an NFA.
--
-- == Requests
-- Functions for querying properties of an NFA, such as its set of states, alphabet, transitions, successors, predecessors, and whether a state is initial or final.
--
-- == Actions of a symbol / a word over a state / set of states
-- Functions for simulating the behavior of the NFA, such as determining the states reached by a symbol or word, and checking if the NFA recognizes a given word.
--
-- == Reversal
-- Functions for reversing the transitions of an NFA, effectively reversing the automaton.
module NFA where

import qualified Data.Foldable as F (Foldable (foldMap'), fold, foldMap, foldl')
import Data.Map (Map)
import qualified Data.Map as Map (adjust, delete, empty, foldMapWithKey, foldlWithKey', fromList, insert, insertWith, keysSet, lookup, map, singleton, toList, unionWith, (!))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (delete, difference, disjoint, empty, filter, fromList, insert, intersection, map, member, singleton, toList, unions)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements, generate, sized, suchThat, vectorOf)

-- * The NFA type

-- | Type to represent the successors of a state for the alphabet symbols, in a Nondeterministic Finite Automaton (NFA)
type Successors state symbol = Map symbol (Set state)

-- | Type to represent the transitions of a Nondeterministic Finite Automaton (NFA)
type Transitions state symbol = Map state (Successors state symbol)

-- | Type to represent a Nondeterministic Finite Automaton (NFA)
data NFA symbol state = NFA
  { -- | The set of initial states
    initial :: Set state,
    -- | The set of final states
    final :: Set state,
    -- | The transition map, associating a state with a map that associates a symbol with a set of states
    delta :: Transitions state symbol,
    -- | The reverse transition map
    delta_rev :: Transitions state symbol
  }
  deriving (Show)

-- * Modification functions

-- | Switchs the initiality of a state in an NFA.
-- If p is an initial state, it remains in the NFA even if it is not useful anymore
switchInit :: (Ord state, Ord symbol) => state -> NFA symbol state -> NFA symbol state
switchInit p nfa =
  if isInitial nfa p
    then nfa {initial = Set.delete p (initial nfa), delta = new_delta, delta_rev = reverseTransitionMap new_delta}
    else nfa {initial = Set.insert p (initial nfa)}
  where
    new_delta = Map.insertWith (\_new_val old_val -> old_val) p (Map.empty) (delta nfa)

-- | Switchs the finality of a state in an NFA.
-- If p is a final state, it remains in the NFA even if it is not useful anymore
switchFinal :: (Ord state, Ord symbol) => state -> NFA symbol state -> NFA symbol state
switchFinal p nfa =
  if isFinal nfa p
    then nfa {final = Set.delete p (final nfa), delta = new_delta, delta_rev = reverseTransitionMap new_delta}
    else nfa {final = Set.insert p (final nfa)}
  where
    new_delta = Map.insertWith (\_new_val old_val -> old_val) p (Map.empty) (delta nfa)

-- | Switchs the existence of a transition (p, a, q) in an NFA
switchTrans :: (Ord state, Ord symbol) => (state, symbol, state) -> NFA symbol state -> NFA symbol state
switchTrans (p, a, q) nfa =
  if q `Set.member` sendsState nfa a p
    then nfa {delta = new_delta, delta_rev = reverseTransitionMap new_delta}
    else nfa {delta = new_delta', delta_rev = reverseTransitionMap new_delta'}
  where
    new_delta = removeTransitionInMap (delta nfa) (p, a, q)
    new_delta' = addTransitionInMap (delta nfa) (p, a, q)

-- | Returns the successors of a given state considering a transition map
getSuccsInTransitionMap :: (Ord state) => Transitions state symbol -> state -> Maybe (Successors state symbol)
getSuccsInTransitionMap = flip Map.lookup

-- | Transforms the successors of a state in a transition map, applying a function to the states
-- unmodifying the symbols
transformsSuccs :: (Ord state') => (state -> state') -> Successors state symbol -> Successors state' symbol
transformsSuccs = Map.map . Set.map

-- | Filters the successors of a state in a transition map, keeping only the states that satisfy a predicate
--  and then apply a transformation function
filterTransformsSuccs :: (Ord state') => (state -> state') -> (state -> Bool) -> Successors state symbol -> Successors state' symbol
filterTransformsSuccs f p = Map.map (Set.map f . Set.filter p)

-- | Replace a state by a set of states in the successors of a state in a transition map
foldMapSuccs :: (Ord state') => (state -> Set state') -> Successors state symbol -> Successors state' symbol
foldMapSuccs f = Map.map $ F.foldMap f

-- | Adds a transition (p, a, q) in a transition Map
addTransitionInMap ::
  (Ord symbol, Ord state) =>
  -- | The transition Map
  Transitions state symbol ->
  -- | The transition
  (state, symbol, state) ->
  -- | The resulting transition Map
  Transitions state symbol
addTransitionInMap trans (p, a, q) = Map.insertWith (Map.unionWith (<>)) p (Map.singleton a (Set.singleton q)) trans

-- | Removes a transition (p, a, q) in a transition Map
removeTransitionInMap ::
  (Ord symbol, Ord state) =>
  -- | The transition Map
  Transitions state symbol ->
  -- | The transition
  (state, symbol, state) ->
  -- | The resulting transition Map
  Transitions state symbol
removeTransitionInMap trans (p, a, q) = Map.adjust (Map.adjust (Set.delete q) a) p trans

-- | Adds to the successors of a state p, by a symbol a, the states in qs
addSuccsInMap ::
  (Ord symbol, Ord state) =>
  -- | The transition Map
  Transitions state symbol ->
  -- | The state p
  state ->
  -- | The symbol a
  symbol ->
  -- | The set of states qs
  Set state ->
  -- | The resulting transition Map
  Transitions state symbol
addSuccsInMap trans p a qs = Map.insertWith (Map.unionWith (<>)) p (Map.singleton a qs) trans

-- | Removes states from the successors of all the states in a transition map
removeInMap :: (Ord state) => Transitions state symbol -> Set state -> Transitions state symbol
removeInMap trans qs = Map.map (Map.map (`Set.difference` qs)) $ F.foldl' (flip Map.delete) trans qs

-- | Removes a set of states and their related transitions
removeStates :: (Ord state) => NFA symbol state -> Set state -> NFA symbol state
removeStates nfa qs = NFA (Set.difference (initial nfa) qs) (Set.difference (final nfa) qs) trans' trans_rev'
  where
    trans' = removeInMap (delta nfa) qs
    trans_rev' = removeInMap (delta_rev nfa) qs

-- | Restricts the successor of a state to the ones contained in a set of state
restrictSuccs :: (Ord state) => state -> Set state -> Transitions state symbol -> Transitions state symbol
restrictSuccs p qs = Map.adjust (Map.map (Set.intersection qs)) p

-- | Relabelled the states of an NFA with n states to be in the range [0..n-1]
renumStates :: (Ord state) => NFA symbol state -> NFA symbol Int
renumStates aut = mapState (the_map Map.!) aut
  where
    the_map = Map.fromList $ zip (Set.toList $ getStates aut) [0 :: Int ..]

-- * The Arbitrary Instance

instance Arbitrary (NFA Char Int) where
  arbitrary = sized $ \n -> do
    let qs = [1 .. n]
    let alphabet = ['a' .. 'e']
    nb_i <- choose (0, n)
    nb_f <- choose (0, n)
    nb_t <- choose (0, n * n * length alphabet)
    is <- vectorOf nb_i $ elements qs
    fs <- vectorOf nb_f $ elements qs
    ts <- vectorOf nb_t $ elements [(p, a, q) | p <- qs, a <- alphabet, q <- qs]
    let rev_ts = [(q, a, p) | (p, a, q) <- ts]
    return $ NFA (Set.fromList is) (Set.fromList fs) (F.foldl' addTransitionInMap Map.empty ts) (F.foldl' addTransitionInMap Map.empty rev_ts)

-- | Creates a generator for a NFA, from a superset of symbols and a superset of states, with bounds for the number of initial states, of final states and of transitions
makeGenNFA ::
  (Ord state, Ord symbol) =>
  -- | The superset of symbols
  [symbol] ->
  -- | The superset of states
  [state] ->
  -- | The maximal number of initial states
  Int ->
  -- | The maximal number of final states
  Int ->
  -- | The maximal number of transitions
  Int ->
  -- | The resulting generator
  Gen (NFA symbol state)
makeGenNFA symbols qs inits finals transitions = do
  is <- fmap Set.fromList $ vectorOf inits $ elements qs
  fs <- fmap Set.fromList $ vectorOf finals $ elements qs
  ts <- vectorOf transitions $ elements [(p, a, q) | p <- qs, a <- symbols, q <- qs]
  let ts_rev = [(q, a, p) | (p, a, q) <- ts]
  return $ NFA is fs (F.foldl' addTransitionInMap Map.empty ts) (F.foldl' addTransitionInMap Map.empty ts_rev)

-- | Creates a generator for a NFA from makeGenNFa, such that the generated NFAs satisfy the predicate p
makeGenNFASuchThat :: (Ord state, Ord symbol) => (NFA symbol state -> Bool) -> [symbol] -> [state] -> Int -> Int -> Int -> Gen (NFA symbol state)
makeGenNFASuchThat p symbols qs inits finals transitions = makeGenNFA symbols qs inits finals transitions `suchThat` p

-- | Generates an NFA using the corresponding makeGenNFA generator
generateNFA :: (Ord state, Ord symbol) => [symbol] -> [state] -> Int -> Int -> Int -> IO (NFA symbol state)
generateNFA symbols qs inits finals transitions = generate $ makeGenNFA symbols qs inits finals transitions

-- | Generates an NFA using the corresponding makeGenNFASuchThat generator
generateNFASuchThat :: (Ord state, Ord symbol) => (NFA symbol state -> Bool) -> [symbol] -> [state] -> Int -> Int -> Int -> IO (NFA symbol state)
generateNFASuchThat p symbols qs inits finals transitions = generate $ makeGenNFASuchThat p symbols qs inits finals transitions

-- * Functorial fmap like

-- | Applies a function over the states of an NFA
mapState ::
  (Ord state') =>
  -- | The modification function f
  (state -> state') ->
  -- | The NFA
  NFA symbol state ->
  -- | The equivalent NFA where any state p is replaced by f p
  NFA symbol state'
mapState f nfa =
  NFA
    { initial = Set.map f (initial nfa),
      final = Set.map f (final nfa),
      delta = Map.foldlWithKey' (\res p a_to_states -> Map.insert (f p) (Set.map f <$> a_to_states) res) Map.empty (delta nfa),
      delta_rev = Map.foldlWithKey' (\res p a_to_states -> Map.insert (f p) (Set.map f <$> a_to_states) res) Map.empty (delta_rev nfa)
    }

-- * Requests

-- | Computes the states that appears in the transition Map, as source or as destination
getStates ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The set of the states that appear in the transition Map
  Set state
getStates nfa = Set.unions [Map.foldMapWithKey (\q -> Set.insert q . F.fold) $ delta nfa, initial nfa, final nfa]

-- | Retuens the symbols that appear in the NFA
getAlphabet ::
  (Ord symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The resulting set
  Set symbol
getAlphabet nfa = F.foldMap Map.keysSet $ delta nfa

-- | Returns the list of the transitions of an NFA
transitionList ::
  -- | The NFA
  NFA symbol state ->
  -- | The transition list
  [(state, symbol, state)]
transitionList nfa = Map.toList (delta nfa) >>= \(p, a_to_states) -> Map.toList a_to_states >>= (\(a, qs) -> (p,a,) <$> Set.toList qs)

-- | Returns the direct successors of a state
getSuccs ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The state p
  state ->
  -- | The direct successors of p
  Set state
getSuccs nfa p = maybe Set.empty F.fold (Map.lookup p (delta nfa))

-- | Returns the direct predecessors of a state
getPreds :: (Ord a) => NFA symbol a -> a -> Set a
getPreds nfa p = maybe Set.empty F.fold (Map.lookup p (delta_rev nfa))

-- | Returns the direct successors of a state, in a couple with the symbol that leads to the state,
-- if the NFA is homogeneous
getSuccsWithSymbol ::
  (Ord state, Ord symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The state p
  state ->
  -- | The direct successors of p
  Set (state, symbol)
getSuccsWithSymbol nfa p = maybe Set.empty (Map.foldMapWithKey (\symb -> Set.map (,symb))) $ Map.lookup p $ delta nfa

-- | Tests whether a state is initial
isInitial ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The state p
  state ->
  -- | The Boolean "p is initial"
  Bool
isInitial nfa p = Set.member p $ initial nfa

-- | Tests whether a state is final
isFinal ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The state p
  state ->
  -- | The Boolean "p is final"
  Bool
isFinal nfa p = Set.member p $ final nfa

-- * Actions of a symbol / a word over a state / set of states

-- | Computes the states of an NFA reached from a state reading a symbol
sendsState ::
  (Ord state, Ord symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The symbol a
  symbol ->
  -- | The state q
  state ->
  -- | The successors of q by a
  Set state
sendsState nfa a q = fromMaybe Set.empty $ Map.lookup q (delta nfa) >>= Map.lookup a

-- | Computes the states of an NFA reached from a set of states  reading a symbol
sendsStateSet ::
  (Ord state, Ord symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The symbol a
  symbol ->
  -- | The set of states qs
  Set state ->
  -- | The successors of the states in qs by a
  Set state
sendsStateSet = F.foldMap' .: sendsState
  where
    -- (.:) f g x y = f $ g x y
    (.:) = (.) . (.)

-- | Computes the states of an NFA reached from a set of states  reading a word
sends ::
  (Ord state, Ord symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The word w
  [symbol] ->
  -- | The set of states qs
  Set state ->
  -- | The successors of the states in qs by w
  Set state
sends = flip . F.foldl' . flip . sendsStateSet

-- | Determines whether a word is recognized by an NFA
recognizes ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The word w
  [symbol] ->
  -- | The Boolean "w is recognized by A"
  Bool
recognizes nfa w = not $ Set.disjoint (sends nfa w $ initial nfa) (final nfa)

-- * Reversal

-- | Computes the reverse of a transition map
reverseTransitionMap :: (Ord symbol, Ord state) => Transitions state symbol -> Transitions state symbol
reverseTransitionMap trans = F.foldl' addTransitionInMap Map.empty ((\(p, a, q) -> (q, a, p)) <$> transitions)
  where
    transitions = Map.toList trans >>= \(p, a_to_states) -> Map.toList a_to_states >>= (\(a, qs) -> (p,a,) <$> Set.toList qs)

-- | Computes the reversal of an NFA
reverse ::
  -- | The NFA A
  NFA symbol state ->
  -- | The reversal of A
  NFA symbol state
reverse nfa = NFA (final nfa) (initial nfa) (delta_rev nfa) (delta nfa)
