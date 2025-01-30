{-# LANGUAGE GADTs #-}

module PSNFA where

import NFA (NFA, renumStates)
import NFAAccessibility (trim)
import NFADotRepr (nfaToDot)
import NFAHomogeneity (makeHomogeneous)
import NFAStandard (makeStandard)
import ToString (ToString)

-- | A PSNFA (Phamtom State NFA) is a wrapper around an NFA that masks the state type
-- but ensures that the states are showable, orderable, and can be converted to strings.
data PSNFA symbol where
  PSNFA :: (Show a, Ord a, ToString a) => NFA symbol a -> PSNFA symbol

-- | Makes a PSNFA homogeneous
makeHomogeneousPS :: (Show symbol, Ord symbol, ToString symbol) => PSNFA symbol -> PSNFA symbol
makeHomogeneousPS (PSNFA nfa) = PSNFA $ makeHomogeneous nfa

-- | Trims a PSNFA
trimPS :: PSNFA symbol -> PSNFA symbol
trimPS (PSNFA nfa) = PSNFA $ trim nfa

-- | Makes a PSNFA standard
makeStandardPS :: (Ord symbol) => PSNFA symbol -> PSNFA symbol
makeStandardPS (PSNFA nfa) = PSNFA $ makeStandard nfa

-- | Converts a PSNFA to a dot representation
nfaToDotPS :: (ToString symbol) => PSNFA symbol -> String
nfaToDotPS (PSNFA nfa) = nfaToDot nfa

-- | Renumbers the states of a n-state PSNFA to be in the range [0 .. n -1]
renumStatesPS :: PSNFA symbol -> PSNFA symbol
renumStatesPS (PSNFA nfa) = PSNFA $ renumStates nfa