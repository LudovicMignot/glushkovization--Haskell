module NFA where

import qualified Data.Foldable as F (Foldable (foldMap'), all, fold, foldl')
import Data.Map (Map)
import qualified Data.Map as Map (empty, foldMapWithKey, foldlWithKey', insert, insertWith, lookup, singleton, unionWith)
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set (disjoint, empty, foldl', fromList, insert, map, singleton, size, union, unions)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, generate, vectorOf)

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

-- * The Arbitrary Instance

instance (Ord state, Ord symbol, Arbitrary symbol, Arbitrary state) => Arbitrary (NFA symbol state) where
  arbitrary = NFA <$> arbitrary <*> arbitrary <*> arbitrary

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
makeGenNFA alpha qs inits finals transitions = do
  is <- fmap Set.fromList $ vectorOf inits $ elements qs
  fs <- fmap Set.fromList $ vectorOf finals $ elements qs
  ts <- vectorOf transitions $ elements [(p, a, q) | p <- qs, a <- alpha, q <- qs]
  return $ NFA is fs $ F.foldl' addTransitionInMap Map.empty ts

-- | Generates an NFA using the corresponding makeGenNFA generator
generateNFA :: (Ord state, Ord symbol) => [symbol] -> [state] -> Int -> Int -> Int -> IO (NFA symbol state)
generateNFA alpha qs inits finals transitions = generate $ makeGenNFA alpha qs inits finals transitions

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

-- * Computation of the states of the NFA

-- | Computes the states that appears in the transition Map, as source or as destination
getStates ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The set of the states that appear in the transition Map
  Set state
getStates nfa = Set.unions [Map.foldMapWithKey (\q -> Set.insert q . F.fold) $ delta nfa, initial nfa, final nfa]

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
      delta = Map.insert Nothing succ_inits delta_just
    }
  where
    -- equivalent to nfa, where the states p are "promoted" as Just p
    nfa_just = mapState Just nfa
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