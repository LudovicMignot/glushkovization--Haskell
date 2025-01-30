{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Warp (run)
import NFA (NFA, generateNFA, renumStates)
import PSNFA (PSNFA (PSNFA), makeHomogeneousPS, makeStandardPS, renumStatesPStoNFA, trimPS)
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
  aut <- liftIO $ Left <$> generateNFA ['a' .. 'e'] [(0 :: Int) .. 10] 2 2 5
  rec aut_dyn <- foldDyn ($) aut $ leftmost [trimPS' <$ evt, makeHomogeneousPS' <$ evt2, makeStandardPS' <$ evt3, renumStatesPS' <$ evt4]
      evt <- labelledButton "trim"
      evt2 <- labelledButton "makeHomogeneous"
      evt3 <- labelledButton "makeStandard"
      evt4 <- labelledButton "renum"
  _ <-
    dyn $
      theContent
        <$> aut_dyn
  footer
  where
    trimPS' (Left aut) = Right $ trimPS $ PSNFA aut
    trimPS' (Right aut) = Right $ trimPS aut
    makeHomogeneousPS' (Left aut) = Right $ makeHomogeneousPS $ PSNFA aut
    makeHomogeneousPS' (Right aut) = Right $ makeHomogeneousPS aut
    makeStandardPS' (Left aut) = Right $ makeStandardPS $ PSNFA aut
    makeStandardPS' (Right aut) = Right $ makeStandardPS aut
    renumStatesPS' (Left aut) = Left $ renumStates aut
    renumStatesPS' (Right aut) = Left $ renumStatesPStoNFA aut

footer :: (DomBuilder t m) => m ()
footer = do
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", bootstrapJsCDN)]) $
    return ()
  where
    bootstrapJsCDN =
      "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"

theContent ::
  ( MonadWidget t m
  ) =>
  Either (NFA Char Int) (PSNFA Char) ->
  m ()
theContent aut =
  elAttr
    "div"
    ("class" =: "d-flex flex-column align-items-center")
    $ svgAut
    $ toPSNFA aut
  where
    toPSNFA (Right auto) = auto
    toPSNFA (Left auto) = PSNFA auto
