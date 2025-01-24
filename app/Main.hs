{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Test (runStabilization)

import qualified Data.Map as Map
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

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
  _ <- elAttr "div" ("class" =: "container") $ do
    elAttr "h1" ("class" =: "text-center") $ text "Word Automata Constructions"
    el "hr" $ return ()
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