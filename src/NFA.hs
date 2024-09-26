{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module NFA where

import qualified Data.Foldable as F (Foldable (foldMap'), all, fold, foldl')
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.Map (Map)
import qualified Data.Map as Map (empty, foldMapWithKey, foldlWithKey', insert, insertWith, lookup, singleton, toList, unionWith)
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set (difference, disjoint, empty, foldl', fromList, insert, map, member, singleton, size, toList, union, unions)
import qualified Data.Text.Lazy as L
import System.Directory
  ( createDirectoryIfMissing,
    getCurrentDirectory,
  )
import System.FilePath (combine)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, generate, vectorOf)
import ToString (ToString, toHtmlCapString, toHtmlString, toString)

-- * The NFA type

-- | Type to represent a Nondeterministic Finite Automaton (NFA)
data NFA symbol state = NFA
  { -- | The set of initial states
    initial :: Set state,
    -- | The set of final states
    final :: Set state,
    -- | The transition map, associating a state with a map that associates a symbol with a set of states
    delta :: Map state (Map symbol (Set state))
  }
  deriving (Show)

-- * Dot representation

-- | Converts an NFA into its dot representation, as a String
nfaToDot ::
  (ToString state, Ord state, ToString symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The dot String
  String
nfaToDot auto = "digraph{" ++ statementList ++ "}"
  where
    statementList = graphs ++ nodes ++ edges
    graphs = "graph [rankdir = LR];"
    nodes = finals ++ nonFinals
    finals = concatMap (\s -> myToString s ++ att1 s) $ Set.toList $ final auto
    nonFinals = concatMap (\s -> myToString s ++ att2 s) $ Set.difference (getStates auto) $ final auto
    edges = concatMap (\(p, x, q) -> myToString p ++ "->" ++ myToString q ++ " [label = <" ++ toHtmlString x ++ ">];") trans
    trans =
      Map.foldlWithKey' (\accu (p, q) x -> (p, x, q) : accu) [] $
        F.foldl' (\accu (p, x, q) -> Map.insertWith (++) (p, q) [x] accu) Map.empty $
          transitionList auto
    myToString p = "\"" ++ toHtmlCapString p ++ "\""
    att1 p
      | isInitial auto p =
          " [shape = octagon, peripheries = 2, style = rounded, style = filled, color = gray35" ++ ", label=<" ++ toHtmlString p ++ ">];"
      | otherwise =
          " [shape = box, peripheries = 2, style = rounded" ++ ", label=<" ++ toHtmlString p ++ ">];"
    att2 p
      | isInitial auto p =
          " [shape = octagon, style = rounded, style = filled, color = gray35" ++ ", label=<" ++ toHtmlString p ++ ">];"
      | otherwise =
          " [shape = box, style = rounded" ++ ", label=<" ++ toHtmlString p ++ ">];"

-- | Converts an NFA into its dot representation, as a DotGraph String, with given name
faToGraphviz ::
  (ToString state, Ord state, ToString symbol) =>
  -- | The name
  L.Text ->
  -- | The NFA
  NFA symbol state ->
  -- | The resulting DotGraph String value
  G.DotGraph String
faToGraphviz name auto =
  digraph (Str name) $ do
    graphAttrs [RankDir FromLeft]
    mapM_ (\s -> node (toString s) (att1 s)) $ final auto
    mapM_ (\s -> node (toString s) (att2 s)) $ Set.difference (getStates auto) $ final auto
    mapM_ (\(p, x, q) -> edge (toString p) (toString q) [toLabel (toString x)]) trans
  where
    trans =
      Map.foldlWithKey' (\accu (p, q) x -> (p, x, q) : accu) [] $
        F.foldl' (\accu (p, x, q) -> Map.insertWith (++) (p, q) [x] accu) Map.empty $
          transitionList auto
    att1 p
      | isInitial auto p = [shape Octagon, Peripheries 2, Style [SItem Rounded [], SItem Filled []], Color (toColorList [RGB 100 100 100])]
      | otherwise = [shape BoxShape, Peripheries 2, Style [SItem Rounded []]]
    att2 p
      | isInitial auto p = [shape Octagon, Style [SItem Rounded [], SItem Filled []], Color (toColorList [RGB 100 100 100])]
      | otherwise = [shape BoxShape, Style [SItem Rounded []]]

-- | Converts an NFA into a PNG file with a given name
toPng ::
  (ToString state, Ord state, ToString symbol) =>
  -- | The name of the file
  String ->
  -- | The NFA
  NFA symbol state ->
  -- | The resulting action
  IO FilePath
toPng name aut = addExtension (runGraphviz autoDot) Png name
  where
    autoDot = faToGraphviz (L.pack name) aut

-- | Converts an NFA into a PNG file with a given name in a given directory
toPngAt ::
  (ToString state, Ord state, ToString symbol) =>
  -- | The name of the directory
  String ->
  -- | The name of the file
  String ->
  -- | The NFA
  NFA symbol state ->
  -- | The resulting action
  IO FilePath
toPngAt dir name aut =
  addExtension (runGraphviz autoDot) Png (combine dir name)
  where
    autoDot = faToGraphviz (L.pack name) aut

-- | Converts an NFA into a PNG file with a given name in a given directory, created if missing
toPngInDir ::
  (ToString state, Ord state, ToString symbol) =>
  -- | The name of the directory
  String ->
  -- | The name of the file
  String ->
  -- | The NFA
  NFA symbol state ->
  -- | The resulting action
  IO FilePath
toPngInDir dir name aut = do
  cur <- getCurrentDirectory
  let img = combine cur dir
  createDirectoryIfMissing False img
  addExtension (runGraphviz autoDot) Png (combine img name)
  where
    autoDot = faToGraphviz (L.pack name) aut

-- | Converts an NFA into a PNG file with a given name in a directory named "img", created if missing
toPngInImgDir ::
  (ToString state, Ord state, ToString symbol) =>
  -- | The name of the file
  String ->
  -- | The NFA
  NFA symbol state ->
  -- | The resulting action
  IO FilePath
toPngInImgDir = toPngInDir "img"

-- * Modification functions

-- | Adds a transition (p, a, q) in a transition Map
addTransitionInMap ::
  (Ord symbol, Ord state) =>
  -- | The transition Map
  Map state (Map symbol (Set state)) ->
  -- | The transition
  (state, symbol, state) ->
  -- | The resulting transition Map
  Map state (Map symbol (Set state))
addTransitionInMap trans (p, a, q) = Map.insertWith (Map.unionWith (<>)) p (Map.singleton a (Set.singleton q)) trans

-- * The Arbitrary Instance

instance (Ord state, Ord symbol, Arbitrary symbol, Arbitrary state) => Arbitrary (NFA symbol state) where
  arbitrary = NFA <$> arbitrary <*> arbitrary <*> arbitrary

-- | Creates a generator for a NFA, from a superset of symbols and a superset of states, with bounds for the number of initial states, of final states and of transitions
makeGenNFA ::
  (Ord state, Ord symbol) =>
  -- | The superset of symbols
  [symbol] ->
  -- | The superset of states
  [state] ->
  -- | The maximal number of initial states
  Int ->
  -- | The maximal number of final states
  Int ->
  -- | The maximal number of transitions
  Int ->
  -- | The resulting generator
  Gen (NFA symbol state)
makeGenNFA symbols qs inits finals transitions = do
  is <- fmap Set.fromList $ vectorOf inits $ elements qs
  fs <- fmap Set.fromList $ vectorOf finals $ elements qs
  ts <- vectorOf transitions $ elements [(p, a, q) | p <- qs, a <- symbols, q <- qs]
  return $ NFA is fs $ F.foldl' addTransitionInMap Map.empty ts

-- | Generates an NFA using the corresponding makeGenNFA generator
generateNFA :: (Ord state, Ord symbol) => [symbol] -> [state] -> Int -> Int -> Int -> IO (NFA symbol state)
generateNFA symbols qs inits finals transitions = generate $ makeGenNFA symbols qs inits finals transitions

-- * Functorial fmap like

-- | Applies a function over the states of an NFA
mapState ::
  (Ord state') =>
  -- | The modification function f
  (state -> state') ->
  -- | The NFA
  NFA symbol state ->
  -- | The equivalent NFA where any state p is replaced by f p
  NFA symbol state'
mapState f nfa =
  NFA
    { initial = Set.map f (initial nfa),
      final = Set.map f (final nfa),
      delta = Map.foldlWithKey' (\res p a_to_states -> Map.insert (f p) (Set.map f <$> a_to_states) res) Map.empty (delta nfa)
    }

-- * Requests

-- | Computes the states that appears in the transition Map, as source or as destination
getStates ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The set of the states that appear in the transition Map
  Set state
getStates nfa = Set.unions [Map.foldMapWithKey (\q -> Set.insert q . F.fold) $ delta nfa, initial nfa, final nfa]

-- | Returns the list of the transitions of an NFA
transitionList ::
  -- | The NFA
  NFA symbol state ->
  -- | The transition list
  [(state, symbol, state)]
transitionList nfa = Map.toList (delta nfa) >>= \(p, a_to_states) -> Map.toList a_to_states >>= (\(a, qs) -> (p,a,) <$> Set.toList qs)

-- | Tests whether a state is initial
isInitial ::
  (Ord state) =>
  -- | The NFA
  NFA symbol state ->
  -- | The state p
  state ->
  -- | The Boolean "p is initial"
  Bool
isInitial nfa p = Set.member p $ initial nfa

-- * Actions of a symbol / a word over a state / set of states

-- | Computes the states of an NFA reached from a state reading a symbol
sendsState ::
  (Ord state, Ord symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The symbol a
  symbol ->
  -- | The state q
  state ->
  -- | The successors of q by a
  Set state
sendsState nfa a q = fromMaybe Set.empty $ Map.lookup q (delta nfa) >>= Map.lookup a

-- | Computes the states of an NFA reached from a set of states  reading a symbol
sendsStateSet ::
  (Ord state, Ord symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The symbol a
  symbol ->
  -- | The set of states qs
  Set state ->
  -- | The successors of the states in qs by a
  Set state
sendsStateSet = F.foldMap' .: sendsState
  where
    -- (.:) f g x y = f $ g x y
    (.:) = (.) . (.)

-- | Computes the states of an NFA reached from a set of states  reading a word
sends ::
  (Ord state, Ord symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The word w
  [symbol] ->
  -- | The set of states qs
  Set state ->
  -- | The successors of the states in qs by w
  Set state
sends = flip . F.foldl' . flip . sendsStateSet

-- | Determines whether a word is recognized by an NFA
recognizes ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The word w
  [symbol] ->
  -- | The Boolean "w is recognized by A"
  Bool
recognizes nfa w = not $ Set.disjoint (sends nfa w $ initial nfa) (final nfa)

-- * Standard NFA

-- | Determines whether an NFA is standard, i.e. if there is only one initial state which is not the destination of a transition
isStandard ::
  (Ord symbol) =>
  -- | The NFA A
  NFA state symbol ->
  -- | The Boolean "A is standard"
  Bool
isStandard nfa = Set.size (initial nfa) == 1 && F.all (F.all $ Set.disjoint $ initial nfa) (delta nfa)

-- Computes an equivalent standard NFA from an NFA by adding a initial state
makeStandard ::
  (Ord state, Ord symbol) =>
  -- | The NFA A
  NFA symbol state ->
  -- | The standard NFA equivalent to A
  NFA symbol (Maybe state)
makeStandard nfa =
  NFA
    { initial = Set.singleton Nothing,
      final = final',
      delta = Map.insert Nothing succ_inits delta_just
    }
  where
    -- equivalent to nfa, where the states p are "promoted" as Just p
    nfa_just = mapState Just nfa
    -- equivalent to delta, where the states p are "promoted" as Just p
    delta_just = delta nfa_just
    -- the successors of the old initial states
    succ_inits = Set.foldl' (\res i -> Maybe.maybe res (Map.unionWith Set.union res) $ Map.lookup i delta_just) Map.empty $ initial nfa_just
    -- the final states p "promoted" as Just p
    just_final = final nfa_just
    -- The new final states, adding the initial states if at least one old initial state was final
    final'
      | Set.disjoint (initial nfa) (final nfa) = just_final
      | otherwise = Set.insert Nothing just_final