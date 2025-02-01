{-# LANGUAGE GADTs #-}

module PSNFA where

import NFA (NFA, renumStates)
import NFAAccessibility (isTrim, trim)
import NFADotRepr (nfaToDot)
import NFAHomogeneity (isHomogeneous, makeHomogeneous)
import NFAOrbit (kosaraju)
import NFAStandard (isStandard, makeStandard)
import ToString (ToString, toString)

-- | A PSNFA (Phamtom State NFA) is a wrapper around an NFA that masks the state type
-- but ensures that the states are showable, orderable, and can be converted to strings.
data PSNFA symbol where
  PSNFA :: (Show a, Ord a, ToString a) => NFA symbol a -> PSNFA symbol

-- | Makes a PSNFA homogeneous
makeHomogeneousPS :: (Show symbol, Ord symbol, ToString symbol) => PSNFA symbol -> PSNFA symbol
makeHomogeneousPS (PSNFA nfa) = PSNFA $ makeHomogeneous nfa

-- | Checks whether a PSNFA is homogeneous
isHomogeneousPS :: (Ord symbol) => PSNFA symbol -> Bool
isHomogeneousPS (PSNFA nfa) = isHomogeneous nfa

-- | Trims a PSNFA
trimPS :: PSNFA symbol -> PSNFA symbol
trimPS (PSNFA nfa) = PSNFA $ trim nfa

-- | Checks whether a PSNFA is trim
isTrimPS :: PSNFA symbol -> Bool
isTrimPS (PSNFA nfa) = isTrim nfa

-- | Makes a PSNFA standard
makeStandardPS :: (Ord symbol) => PSNFA symbol -> PSNFA symbol
makeStandardPS (PSNFA nfa) = PSNFA $ makeStandard nfa

-- | Checks whether a PSNFA is standard
isStandardPS :: PSNFA symbol -> Bool
isStandardPS (PSNFA nfa) = isStandard nfa

-- | Converts a PSNFA to a dot representation
nfaToDotPS :: (ToString symbol) => PSNFA symbol -> String
nfaToDotPS (PSNFA nfa) = nfaToDot nfa

-- | Renumbers the states of a n-state PSNFA to be in the range [0 .. n -1]
renumStatesPStoNFA :: PSNFA symbol -> NFA symbol Int
renumStatesPStoNFA (PSNFA nfa) = renumStates nfa

-- | Returns the orbits of a PSNFA as a list of lists of Text
kosarajuStringPS :: PSNFA symbol -> [[String]]
kosarajuStringPS (PSNFA nfa) = (toString <$>) <$> kosaraju nfa