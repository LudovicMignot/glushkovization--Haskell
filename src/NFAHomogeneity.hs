{-# LANGUAGE TupleSections #-}

module NFAHomogeneity where

import qualified Data.Foldable as F (foldMap)
import Data.List (groupBy, nub, sort)
import qualified Data.Map as Map (foldMapWithKey, mapWithKey, singleton)
import qualified Data.Set as Set (insert, map)
import NFA (NFA (NFA, delta), final, generateNFASuchThat, getAlphabet, initial, reverseTransitionMap, transitionList)

-- * Homogeneous NFA

-- | Determines whether an NFA is homogeneous, i.e., if any two transitions entering the same state are labelled equally
isHomogeneous ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The Boolean "A is homogeneous"
  Bool
isHomogeneous nfa = all (null . tail . nub) $ groupBy (\(p, _) (q, _) -> p == q) $ sort $ (\(_, a, q) -> (q, a)) <$> transitionList nfa

-- | Computes an equivalent homogeneous NFA from an NFA by cloning the states w.r.t. the alphabet of the NFA
makeHomogeneous ::
  (Ord symbol, Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The resulting homogeneous NFA
  NFA symbol (state, Maybe symbol)
makeHomogeneous nfa =
  NFA inits finals new_trans (reverseTransitionMap new_trans)
  where
    sigma = getAlphabet nfa
    sigma_m = Set.insert Nothing $ Set.map Just sigma
    inits = F.foldMap (\i -> Set.map (i,) sigma_m) $ initial nfa
    finals = F.foldMap (\f -> Set.map (f,) sigma_m) $ final nfa
    new_succs = Map.mapWithKey (\a succs -> Set.map (,Just a) succs)
    new_trans =
      Map.foldMapWithKey
        ( \p a_to_states ->
            let newSuccs = new_succs a_to_states in F.foldMap (\a -> Map.singleton (p, a) newSuccs) sigma_m
        )
        $ delta nfa

-- | Generates an homogeneous NFA using the corresponding makeGenNFA generator
generateHomogeneousNFA :: (Ord state, Ord symbol) => [symbol] -> [state] -> Int -> Int -> Int -> IO (NFA symbol state)
generateHomogeneousNFA = generateNFASuchThat isHomogeneous