module NFAComplete where

import qualified Data.Foldable as F
import qualified Data.Map as Map (singleton)
import qualified Data.Set as Set (map, null, singleton)
import NFA
  ( NFA (NFA, final, initial),
    getAlphabet,
    getStates,
    sendsState,
  )

-- | Completes an NFA
complete ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The complete NFA associated with A
  NFA symbol (Maybe state)
complete nfa = NFA (Set.map Just $ initial nfa) (Set.map Just $ final nfa) delta'
  where
    alpha = getAlphabet nfa
    nothing_if_empty e = if Set.null e then Set.singleton Nothing else Set.map Just e
    delta' =
      Map.singleton Nothing (F.foldMap (\a -> Map.singleton a $ Set.singleton Nothing) alpha)
        <> F.foldMap (\p -> Map.singleton (Just p) $ F.foldMap (\a -> Map.singleton a (nothing_if_empty $ sendsState nfa a p)) alpha) (getStates nfa)
