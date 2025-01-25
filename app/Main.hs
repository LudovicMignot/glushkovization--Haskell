{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Test (runStabilization)

import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import Data.GraphViz.Commands
  ( GraphvizCommand (Dot),
    GraphvizOutput (Svg),
    graphvizWithHandle,
  )
import Data.GraphViz.Types (parseDotGraph)
import Data.GraphViz.Types.Generalised as G (DotGraph)
import qualified Data.Map as Map
import Data.Text as Te
  ( Text,
    pack,
    unpack,
  )
import qualified Data.Text.Lazy as Tel
import Language.Javascript.JSaddle.Warp (run)
import NFA (NFA (NFA), generateNFA)
import NFADotRepr (nfaToDot)
import Reflex.Dom.Core
import ToString (ToString (toString))

-- import Reflex.Dom.Core (run)

-- main :: IO ()
-- main = runStabilization

main :: IO ()
main = run 3911 $ mainWidgetWithHead header body

header :: (MonadWidget t m) => m ()
header =
  elAttr
    "link"
    (Map.fromList [("rel", "stylesheet"), ("href", bootstrapCSSCDN)])
    $ return ()
  where
    bootstrapCSSCDN =
      "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"

body :: (MonadWidget t m) => m ()
body = do
  aut <- liftIO $ generateNFA ['a' .. 'e'] [(0 :: Int) .. 10] 2 2 5
  _ <- elAttr "div" ("class" =: "container") $ do
    elAttr "h1" ("class" =: "text-center") $ text "Word Automata Constructions"
    el "hr" $ return ()

  _ <-
    elAttr
      "div"
      ("class" =: "d-flex flex-column align-items-center")
      $ svgAut aut
  footer

footer :: (MonadWidget t m) => m ()
footer = do
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", jqueryCDN)]) $
    return ()
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", popperCDN)]) $
    return ()
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", bootstrapJsCDN)]) $
    return ()
  where
    jqueryCDN = "https://code.jquery.com/jquery-3.3.1.min.js"
    popperCDN =
      "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
    bootstrapJsCDN =
      "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/js/bootstrap.bundle.js"

svgAut ::
  ( MonadWidget t m,
    ToString symbol,
    ToString state,
    Ord state
  ) =>
  NFA symbol state ->
  m ()
svgAut auto = do
  let getData handle = do
        bytes <- BS.hGetContents handle
        return $ Te.pack $ toString bytes
  svg <- liftIO $ graphvizWithHandle Dot (parseDotGraph $ Tel.pack $ nfaToDot auto :: G.DotGraph String) Svg getData
  void $ elDynHtml' "div" $ return svg