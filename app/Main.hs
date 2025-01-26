{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Warp (run)
import NFA (NFA, generateNFA)
import NFAAccessibility (trim)
import Reflex.Dom.Core
  ( DomBuilder,
    MonadHold (holdDyn),
    MonadWidget,
    mainWidgetWithHead,
    (=:),
  )
import Reflex.Dom.Widget
import ToString (ToString)
import Widget

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
  rec aut_dyn <- holdDyn aut $ fmap (\_ -> trim aut) evt
      evt <- labelledButton "trim"
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
    ToString symbol,
    ToString state,
    Ord state
  ) =>
  NFA symbol state ->
  m ()
theContent aut =
  elAttr
    "div"
    ("class" =: "d-flex flex-column align-items-center")
    $ svgAut aut
