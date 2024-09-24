module NFA where

import qualified Data.Foldable as F (all, foldl')
import Data.Map (Map)
import qualified Data.Map as Map (foldMapWithKey, foldl', lookup)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (empty, foldl', insert, intersection, null, size, union)

data NFA symbol state = NFA
  { sigma :: symbol,
    initial :: Set state,
    final :: Set state,
    delta :: Map state (Map symbol (Set state))
  }

getStates :: (Ord state) => NFA symbol state -> Set state
getStates = Map.foldMapWithKey (\q -> Set.insert q . Map.foldl' Set.union Set.empty) . delta

sendsState :: (Ord state, Ord symbol) => NFA symbol state -> symbol -> state -> Set state
sendsState nfa a q = fromMaybe Set.empty $ Map.lookup q (delta nfa) >>= Map.lookup a

sendsStateSet :: (Ord state, Ord symbol) => NFA symbol state -> symbol -> Set state -> Set state
sendsStateSet nfa a =
  Set.foldl' (\res q -> Set.union res $ sendsState nfa a q) Set.empty

sends :: (Ord state, Ord symbol) => NFA symbol state -> [symbol] -> Set state -> Set state
sends nfa = flip $ F.foldl' $ flip $ sendsStateSet nfa

recognizes :: (Ord state, Ord symbol) => NFA symbol state -> [symbol] -> Bool
recognizes nfa w = not $ Set.null $ Set.intersection (sends nfa w $ initial nfa) (final nfa)

isStandard :: (Ord symbol) => NFA state symbol -> Bool
isStandard nfa = Set.size (initial nfa) == 1 && F.all (F.all $ \dests -> Set.null $ Set.intersection dests $ initial nfa) (delta nfa)