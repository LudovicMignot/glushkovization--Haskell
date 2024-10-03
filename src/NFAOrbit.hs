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
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, insert, member, toList)
import NFA (NFA, getStates, getSuccs, reverse)

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