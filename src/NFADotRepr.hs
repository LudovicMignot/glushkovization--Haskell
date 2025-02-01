module NFADotRepr where

import qualified Data.Foldable as F (foldl')
import Data.GraphViz.Attributes
  ( Shape (BoxShape, Octagon),
    shape,
    toLabel,
  )
import Data.GraphViz.Attributes.Complete
  ( Attribute (Color, Peripheries, RankDir, Style),
    Color (RGB),
    RankDir (FromLeft),
    StyleItem (SItem),
    StyleName (Filled, Rounded),
    toColorList,
  )
import Data.GraphViz.Commands
  ( GraphvizOutput (Png),
    addExtension,
    runGraphviz,
  )
import Data.GraphViz.Types.Generalised as G
  ( DotGraph,
    GraphID (Str),
  )
import Data.GraphViz.Types.Monadic
  ( digraph,
    edge,
    graphAttrs,
    node,
  )
import qualified Data.Map as Map (empty, foldlWithKey', insertWith)
import qualified Data.Set as Set (difference, fromList, intersection, member, toList, unions)
import qualified Data.Text.Lazy as L
import NFA (NFA (final), getStates, isInitial, transitionList)
import NFAOrbit (ingates, kosaraju, outgates)
import System.Directory
  ( createDirectoryIfMissing,
    getCurrentDirectory,
  )
import System.FilePath (combine)
import ToString (ToString, toHtmlCapString, toHtmlString, toString)

-- * Dot representation

-- | Converts an NFA into its dot representation, as a String
nfaToDot ::
  (ToString state, Ord state, ToString symbol) =>
  -- | The NFA
  NFA symbol state ->
  -- | The dot String
  String
nfaToDot auto = "digraph{" ++ statementList ++ subgraphs ++ "}"
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
          " [shape = octagon, peripheries = 2, style = rounded, style = filled, fillcolor = gray35" ++ border p ++ ", label=<" ++ toHtmlString p ++ ">];"
      | otherwise =
          " [shape = box, peripheries = 2, style = rounded" ++ border p ++ ", label=<" ++ toHtmlString p ++ ">];"
    att2 p
      | isInitial auto p =
          " [shape = octagon, style = rounded, style = filled, fillcolor = gray35" ++ border p ++ ", label=<" ++ toHtmlString p ++ ">];"
      | otherwise =
          " [shape = box, style = rounded" ++ border p ++ ", label=<" ++ toHtmlString p ++ ">];"
    border p
      | Set.member p in_outgs = ", color = purple"
      | Set.member p ings = ", color = blue"
      | Set.member p outgs = ", color = red"
      | otherwise = ""
    orbits = kosaraju auto
    ings = Set.unions $ ingates auto . Set.fromList <$> orbits
    outgs = Set.unions $ outgates auto . Set.fromList <$> orbits
    in_outgs = ings `Set.intersection` outgs
    orbit_to_sub i o = "subgraph cluster_" ++ show i ++ " {color = \"blue\"; label = \"Orbit " ++ show i ++ "\"; " ++ concatMap (\s -> myToString s ++ ";") o ++ "} "
    subgraphs = concatMap (\(i, o) -> orbit_to_sub i o) $ zip [1 :: Int ..] orbits

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