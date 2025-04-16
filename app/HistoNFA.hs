module HistoNFA where

import qualified Data.Set as Set
import NFA (NFA, getStates, renumStates, switchFinal, switchInit, switchTrans)
import NFAAccessibility (trim)
import NFAOrbit (externalIsolation, internalIsolation, kosarajuSet, orbitalNFA, outgates, stabilizeOrbit, strongStabilizationNFA)
import PSNFA (PSNFA (PSNFA), makeHomogeneousPS, makeStandardPS, renumStatesPStoNFA, trimPS)

-- | An Histo automaton is composed of the previous automata, the current automaton and the next automata
-- The previous and next automata are used to store the history of the automaton
type Histo =
  ( [Either (NFA Char Int) (PSNFA Char)],
    Either (NFA Char Int) (PSNFA Char),
    [Either (NFA Char Int) (PSNFA Char)]
  )

-- | Creates a new Histo automaton with the given automaton as the current automaton
newHisto :: Either (NFA Char Int) (PSNFA Char) -> Histo
newHisto aut = ([], aut, [])

-- | Stabilizes the orbit that contains the state i if it is not Nothing in the current automaton of an histo automaton
-- if the automaton is an NFA. Otherwise it does nothing.
stabOrbNFA :: Maybe Int -> Histo -> Histo
stabOrbNFA (Just i) (before, Left aut, _after) = (Left aut : before, Right $ PSNFA $ stabilizeOrbit aut orbit_i, [])
  where
    orbit_i = Set.unions $ Set.filter (i `Set.member`) $ kosarajuSet aut
stabOrbNFA _m_int au = au

-- | Computes the orbital automaton associated with the orbit in the current automaton of an histo automaton
-- that contains i (if it is not Nothing) if the automaton is an NFA. Otherwise it does nothing
orbNFA :: Maybe Int -> Histo -> Histo
orbNFA (Just i) (before, Left aut, _after) = (Left aut : before, Right $ PSNFA $ orbitalNFA aut orbit_i, [])
  where
    orbit_i = Set.unions $ Set.filter (i `Set.member`) $ kosarajuSet aut
orbNFA _m_int au = au

-- | Externally isolates the orbit containing a state i if it is not Nothing in the current automaton of an histo automaton
-- if the automaton is an NFA. Otherwise it does nothing.
extIsol' :: Maybe Int -> Histo -> Histo
extIsol' (Just i) (before, Left aut, _after)
  | i `Set.member` (getStates aut `Set.intersection` outgates aut orbit_i) = (Left aut : before, Right $ PSNFA $ externalIsolation aut orbit_i i, [])
  where
    orbit_i = Set.unions $ Set.filter (i `Set.member`) $ kosarajuSet aut
extIsol' _m_int au = au

-- | Internally isolates the orbit containing a state i if it is not Nothing in the current automaton of an histo automaton
-- if the automaton is an NFA. Otherwise it does nothing.
intIsol' :: Maybe Int -> Histo -> Histo
intIsol' (Just i) (before, Left aut, _after)
  | i `Set.member` (getStates aut) = (Left aut : before, Right $ PSNFA $ fst $ internalIsolation aut orbit_i i, [])
  where
    orbit_i = Set.unions $ Set.filter (i `Set.member`) $ kosarajuSet aut
intIsol' _m_int au = au

-- | Switch the "initiality" of a state i if it is not Nothing in the current automaton of an histo automaton
-- if the automaton is an NFA. Otherwise it does nothing.
switchInit' :: Maybe Int -> Histo -> Histo
switchInit' (Just i) (before, Left aut, _after) = (Left aut : before, Left $ switchInit i aut, [])
switchInit' _m_int au = au

-- | Switch the "initiality" of a list of states if it is not Nothing in the current automaton of an histo automaton
switchInits' :: Maybe [Int] -> Histo -> Histo
switchInits' (Just is) (before, Left aut, _after) = (Left aut : before, Left $ foldr switchInit aut is, [])
switchInits' _ histo = histo

-- | Switch the "finality" of a state i if it is not Nothing in the current automaton of an histo automaton
-- if the automaton is an NFA. Otherwise it does nothing.
switchFinal' :: Maybe Int -> Histo -> Histo
switchFinal' (Just i) (before, Left aut, _after) = (Left aut : before, Left $ switchFinal i aut, [])
switchFinal' _m_int au = au

-- | Switch the "finality" of a list of states if it is not Nothing in the current automaton of an histo automaton
switchFinals' :: Maybe [Int] -> Histo -> Histo
switchFinals' (Just is) (before, Left aut, _after) = (Left aut : before, Left $ foldr switchFinal aut is, [])
switchFinals' _ histo = histo

-- | Switch the existance of a transition (i, c, j) if it is not Nothing in the current automaton of an histo automaton
-- if the automaton is an NFA. Otherwise it does nothing.
switchTrans' :: Maybe (Int, Char, Int) -> Histo -> Histo
switchTrans' (Just t) (before, Left aut, _after) = (Left aut : before, Left $ switchTrans t aut, [])
switchTrans' _m_int au = au

switchTranss' :: Maybe [(Int, Char, Int)] -> Histo -> Histo
switchTranss' (Just ts) (before, Left aut, _after) = (Left aut : before, Left $ foldr switchTrans aut ts, [])
switchTranss' _ histo = histo

-- | Trims the current automaton of an histo automaton
trimPS' :: Histo -> Histo
trimPS' (before, Left aut, _after) = (Left aut : before, Left $ trim aut, [])
trimPS' (before, Right aut, _after) = (Right aut : before, Right $ trimPS aut, [])

-- | Homogeneizes the current automaton of an histo automaton
makeHomogeneousPS' :: Histo -> Histo
makeHomogeneousPS' (before, Left aut, _after) = (Left aut : before, Right $ trimPS . makeHomogeneousPS $ PSNFA aut, [])
makeHomogeneousPS' (before, Right aut, _after) = (Right aut : before, Right $ trimPS $ makeHomogeneousPS aut, [])

-- | Standardizes the current automaton of an histo automaton
makeStandardPS' :: Histo -> Histo
makeStandardPS' (before, Left aut, _after) = (Left aut : before, Right $ trimPS . makeStandardPS $ PSNFA aut, [])
makeStandardPS' (before, Right aut, _after) = (Right aut : before, Right $ trimPS $ makeStandardPS aut, [])

-- | Renumbers the states of the current automaton of an histo automaton
renumStatesPS' :: Histo -> Histo
renumStatesPS' (before, Left aut, _after) = (Left aut : before, Left $ renumStates aut, [])
renumStatesPS' (before, Right aut, _after) = (Right aut : before, Left $ renumStatesPStoNFA aut, [])

-- | Stabilizes the orbits of the current automaton of an histo automaton
stabPS' :: Histo -> Histo
stabPS' (before, Left aut, _after) = (Left aut : before, Right $ PSNFA $ strongStabilizationNFA aut, [])
stabPS' (before, auto@(Right (PSNFA aut)), _after) = (auto : before, Right $ PSNFA $ strongStabilizationNFA aut, [])

-- | Shifts an histo automaton to the previous state
prec' :: Histo -> Histo
prec' (before, aut, after) = case before of
  [] -> (before, aut, after)
  (x : xs) -> (xs, x, aut : after)

-- | Shifts an histo automaton to the next state
next' :: Histo -> Histo
next' (before, aut, after) = case after of
  [] -> (before, aut, after)
  (x : xs) -> (aut : before, x, xs)