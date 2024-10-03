{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module NFA where

import qualified Data.Foldable as F (Foldable (foldMap'), fold, foldMap, foldl')
import Data.Map (Map)
import qualified Data.Map as Map (delete, empty, foldMapWithKey, foldlWithKey', insert, insertWith, keysSet, lookup, map, singleton, toList, unionWith)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (difference, disjoint, empty, fromList, insert, map, member, singleton, toList, unions)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements, generate, sized, suchThat, vectorOf)

-- * The NFA type

-- | Type to represent a Nondeterministic Finite Automaton (NFA)
data NFA symbol state = NFA
  { -- | The set of initial states
    initial :: Set state,
    -- | The set of final states
    final :: Set state,
    -- | The transition map, associating a state with a map that associates a symbol with a set of states
    delta :: Map state (Map symbol (Set state))
  }
  deriving (Show)

-- * Modification functions

-- | Adds a transition (p, a, q) in a transition Map
addTransitionInMap ::
  (Ord symbol, Ord state) =>
  -- | The transition Map
  Map state (Map symbol (Set state)) ->
  -- | The transition
  (state, symbol, state) ->
  -- | The resulting transition Map
  Map state (Map symbol (Set state))
addTransitionInMap trans (p, a, q) = Map.insertWith (Map.unionWith (<>)) p (Map.singleton a (Set.singleton q)) trans

-- | Remove a set of states and their related transitions
removeStates :: (Ord state) => NFA symbol state -> Set state -> NFA symbol state
removeStates nfa qs = NFA (Set.difference (initial nfa) qs) (Set.difference (final nfa) qs) trans'
  where
    trans' = Map.map (Map.map (`Set.difference` qs)) $ F.foldl' (flip Map.delete) (delta nfa) qs

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
    return $ NFA (Set.fromList is) (Set.fromList fs) $ F.foldl' addTransitionInMap Map.empty ts

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
  return $ NFA is fs $ F.foldl' addTransitionInMap Map.empty ts

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
      delta = Map.foldlWithKey' (\res p a_to_states -> Map.insert (f p) (Set.map f <$> a_to_states) res) Map.empty (delta nfa)
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

-- | Computes the reversal of an NFA
reverse ::
  (Ord symbol, Ord state) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The reversal of A
  NFA symbol state
reverse nfa = NFA (final nfa) (initial nfa) trans'
  where
    trans' = F.foldl' addTransitionInMap Map.empty ((\(p, a, q) -> (q, a, p)) <$> transitionList nfa)
