module NFAOrbit where

import Control.Monad (unless)
import Control.Monad.Extra (concatMapM)
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
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (delete, difference, empty, filter, foldl', fromList, insert, intersection, map, member, singleton, toList)
import NFA (NFA (NFA, final), delta, getStates, getSuccs, initial, isFinal, isInitial, mapState, reverse)

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
kosaraju2 :: (Ord state, Ord symbol) => NFA symbol state -> [state] -> [[state]]
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
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The orbits of A
  [[state]]
kosaraju nfa = kosaraju2 nfa $ kosaraju1 nfa

-- | Computes the orbits of an NFA following the Kosaraju's Algorithm, as a set of set of states
kosarajuSet ::
  (Ord state, Ord symbol) =>
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
outgates nfa orbit = Set.filter (\p -> isFinal nfa p || F.any (`Set.member` orbit) (getSuccs nfa p)) orbit

-- | For a given set O of states and a given state g, adds a clone of O where g is the only state which is final or linked with the outside of O.
-- The original O is not final nor linked with the outside of O anymore.
-- If O is an orbit and g a state of O, performs the external isolation of O
externalIsolation ::
  (Ord state) =>
  -- | The NFA A
  NFA symbol state ->
  -- | A (possibly) orbit O of A
  Set state ->
  -- | A state g
  state ->
  -- | The (possible) external isolation of g
  NFA symbol (Either state state)
externalIsolation nfa orbit g = NFA initial' final' delta'
  where
    toRight (Left s) = Right s
    toRight s = s
    orbit_l = Set.map Left orbit
    others_l = Set.map Left $ getStates nfa `Set.difference` orbit
    nfa' = mapState Left nfa
    initial'
      | isInitial nfa g = Set.insert (Right g) $ initial nfa'
      | otherwise = initial nfa'
    final'
      | isFinal nfa g = Set.insert (Right g) $ Set.delete (Left g) $ final nfa'
      | otherwise = final nfa'
    delta' =
      delta nfa'
        -- successors of "old" g are restricted to the "old" orbit
        & Map.adjust (Map.map (Set.intersection orbit_l)) (Left g)
        -- successors of "new" g are the "new" ones in the clone, and the "old" external ones
        & Map.insert (Right g) (maybe Map.empty (Map.map (Set.map (\s -> if Set.member s orbit_l then toRight s else s))) (Map.lookup (Left g) (delta nfa')))
        -- successors of "new" o' are restricted to the "new" orbit, successors of "old" o are not modified
        & flip (Set.foldl' (\trans o -> Map.insert (Right o) (maybe Map.empty (Map.map (Set.map Right . Set.filter (`Set.member` orbit))) (Map.lookup o (delta nfa))) trans)) (Set.delete g orbit)
        -- for the other "old" states, add a link to o' if o is a successor
        & flip (Set.foldl' (flip (Map.adjust (Map.map (F.foldMap (\p -> if Set.member p orbit_l then Set.fromList [p, toRight p] else Set.singleton p)))))) others_l