{-# LANGUAGE GADTs #-}

module PSNFA where

import NFA (NFA, renumStates)
import NFAAccessibility (trim)
import NFADotRepr (nfaToDot)
import NFAHomogeneity (makeHomogeneous)
import NFAStandard (makeStandard)
import ToString (ToString)

data PSNFA symbol where
  PSNFA :: (Show a, Ord a, ToString a) => NFA symbol a -> PSNFA symbol

makeHomogeneousPS :: (Show symbol, Ord symbol, ToString symbol) => PSNFA symbol -> PSNFA symbol
makeHomogeneousPS (PSNFA nfa) = PSNFA $ makeHomogeneous nfa

trimPS :: PSNFA symbol -> PSNFA symbol
trimPS (PSNFA nfa) = PSNFA $ trim nfa

makeStandardPS :: (Ord symbol) => PSNFA symbol -> PSNFA symbol
makeStandardPS (PSNFA nfa) = PSNFA $ makeStandard nfa

nfaToDotPS :: (ToString symbol) => PSNFA symbol -> String
nfaToDotPS (PSNFA nfa) = nfaToDot nfa

renumStatesPS :: PSNFA symbol -> PSNFA symbol
renumStatesPS (PSNFA nfa) = PSNFA $ renumStates nfa