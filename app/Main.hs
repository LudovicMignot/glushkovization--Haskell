{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Warp (run)
import NFA (generateNFA)
import PSNFA (PSNFA (PSNFA), makeHomogeneousPS, makeStandardPS, renumStatesPS, trimPS)
import Reflex.Dom.Core
  ( DomBuilder,
    MonadWidget,
    foldDyn,
    leftmost,
    mainWidgetWithHead,
    (=:),
  )
import Reflex.Dom.Widget (dyn, el, elAttr, text)
import ToString (ToString)
import Widget (labelledButton, svgAut)

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
  aut <- liftIO $ PSNFA <$> generateNFA ['a' .. 'e'] [(0 :: Int) .. 10] 2 2 5
  rec aut_dyn <- foldDyn (\f auto -> f auto) aut $ leftmost [((\() -> trimPS) <$> evt), ((\() -> makeHomogeneousPS) <$> evt2), ((\() -> makeStandardPS) <$> evt3), ((\() -> renumStatesPS) <$> evt4)]
      evt <- labelledButton "trim"
      evt2 <- labelledButton "makeHomogeneous"
      evt3 <- labelledButton "makeStandard"
      evt4 <- labelledButton "renum"
  _ <-
    dyn $
      theContent
        <$> aut_dyn
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

theContent ::
  ( MonadWidget t m,
    ToString symbol
  ) =>
  PSNFA symbol ->
  m ()
theContent aut =
  elAttr
    "div"
    ("class" =: "d-flex flex-column align-items-center")
    $ svgAut aut
