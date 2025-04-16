{-# LANGUAGE GADTs #-}

-- |
-- Module      : PSNFA
-- Description : Provides a wrapper around NFAs (Non-deterministic Finite Automata) with phantom states and utility functions.
--
-- This module defines the `PSNFA` type, which is a wrapper around an `NFA` (Non-deterministic Finite Automaton)
-- that hides the state type while ensuring that the states are showable, orderable, and convertible to strings.
-- It provides utility functions to manipulate and analyze `PSNFA` instances, including operations for
-- homogeneity, trimming, standardization, state renumbering, and stability analysis.
--
-- The key features of this module include:
--
-- * **Homogeneity**: Functions to check and enforce homogeneity of a `PSNFA`.
-- * **Trimming**: Functions to trim unreachable or non-co-accessible states from a `PSNFA`.
-- * **Standardization**: Functions to check and enforce standardization of a `PSNFA`.
-- * **State Renumbering**: A function to renumber the states of a `PSNFA` to a contiguous range.
-- * **Graph Representation**: A function to convert a `PSNFA` to a DOT representation for visualization.
-- * **Orbit Analysis**: A function to compute the orbits of a `PSNFA` using Kosaraju's algorithm.
-- * **Stability Analysis**: Functions to check whether a `PSNFA` is isolated, stable, or strongly stable.
--
-- This module relies on several other modules for its functionality:
--
-- * `NFA`: Provides the core `NFA` type and basic operations.
-- * `NFAAccessibility`: Provides functions for trimming and checking trimness.
-- * `NFADotRepr`: Provides functions for converting NFAs to DOT representations.
-- * `NFAHomogeneity`: Provides functions for checking and enforcing homogeneity.
-- * `NFAOrbit`: Provides functions for orbit analysis and stability checks.
-- * `NFAStandard`: Provides functions for checking and enforcing standardization.
-- * `ToString`: Provides a typeclass for converting values to strings.
--
-- The `PSNFA` type and its associated functions are designed to simplify the manipulation of NFAs while
-- abstracting away the details of the state type.
module PSNFA where

import NFA (NFA, renumStates)
import NFAAccessibility (isTrim, trim)
import NFADotRepr (nfaToDot)
import NFAHomogeneity (isHomogeneous, makeHomogeneous)
import NFAOrbit (isIsolatedNFA, isStableNFA, isStronglyStableNFA, kosaraju)
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

-- | Checks whether a PSNFA is isolated
isIsolatedNFAPS :: PSNFA symbol -> Bool
isIsolatedNFAPS (PSNFA nfa) = isIsolatedNFA nfa

-- | Checks whether a PSNFA is stable
isStableNFAPS :: PSNFA symbol -> Bool
isStableNFAPS (PSNFA nfa) = isStableNFA nfa

-- | Checks whether a PSNFA is strongly stable
isStronglyStableNFAPS :: (Ord symbol, Show symbol) => PSNFA symbol -> Bool
isStronglyStableNFAPS (PSNFA nfa) = isStronglyStableNFA nfa