{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad (void)
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
  )
import qualified Data.Text.Lazy as Tel
import Language.Javascript.JSaddle.Warp (run)
import NFA (NFA, generateNFA)
import NFAAccessibility (trim)
import NFADotRepr (nfaToDot)
import NFAHomogeneity (makeHomogeneous)
import NFAStandard (makeStandard)
import Reflex.Dom.Core
  ( DomBuilder,
    EventName (Click),
    MonadHold (holdDyn),
    MonadWidget,
    Reflex (Event),
    domEvent,
    el,
    elAttr,
    elAttr',
    elDynHtml',
    foldDyn,
    mainWidgetWithHead,
    text,
    (=:),
  )
import Reflex.Dom.Widget
import ToString (ToString (toString))

main :: IO ()
main = run 3911 $ mainWidgetWithHead header body

header :: (DomBuilder t m) => m ()
header =
  elAttr
    "link"
    (Map.fromList [("rel", "stylesheet"), ("href", bootstrapCSSCDN)])
    $ return ()
  where
    bootstrapCSSCDN =
      "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"

body :: (MonadWidget t m) => m ()
body = do
  _ <- elAttr "div" ("class" =: "container") $ do
    elAttr "h1" ("class" =: "text-center") $ text "Word Automata Constructions"
    el "hr" $ return ()
  aut <- liftIO $ generateNFA ['a' .. 'e'] [(0 :: Int) .. 10] 2 2 5
  rec aut_dyn <- foldDyn (\_ auto -> trim auto) aut evt
      _ <-
        dyn $
          ( elAttr
              "div"
              ("class" =: "d-flex flex-column align-items-center")
              . svgAut
          )
            <$> aut_dyn
      evt <- labelledButton "Make Standard"
  footer

footer :: (DomBuilder t m) => m ()
footer = do
  -- elAttr "script" (Map.fromList [("defer", "defer"), ("src", jqueryCDN)]) $
  --   return ()
  -- elAttr "script" (Map.fromList [("defer", "defer"), ("src", popperCDN)]) $
  --   return ()
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", bootstrapJsCDN)]) $
    return ()
  where
    -- jqueryCDN =  "https://code.jquery.com/jquery-3.3.1.min.js"
    -- popperCDN =
    --    "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
    bootstrapJsCDN =
      "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"

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

labelledButton :: (MonadWidget t m) => Text -> m (Event t ())
labelledButton lbl = do
  (e, _) <- elAttr' "div" ("class" =: "btn btn-primary mx-1") $ text lbl
  return $ () <$ domEvent Click e