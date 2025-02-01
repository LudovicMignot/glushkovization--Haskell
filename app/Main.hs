{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isLeft)
import qualified Data.Map as Map
import Data.Text (pack)
import Language.Javascript.JSaddle.Warp (run)
import NFA (NFA, generateNFA, renumStates, switchFinal, switchInit, switchTrans)
import NFAAccessibility (isTrim, trim)
import NFAHomogeneity (isHomogeneous)
import NFAOrbit (kosaraju)
import NFAStandard (isStandard)
import PSNFA (PSNFA (PSNFA), isHomogeneousPS, isStandardPS, isTrimPS, kosarajuStringPS, makeHomogeneousPS, makeStandardPS, renumStatesPStoNFA, trimPS)
import Reflex (constDyn)
import Reflex.Dom.Core (DomBuilder, MonadWidget, dynText, foldDyn, leftmost, mainWidgetWithHead, (=:))
import Reflex.Dom.Widget (dyn, el, elAttr, text)
import ToString (toString)
import Widget (labelledButton, lecteurInt, lecteurTrans, svgAut)

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
  rec aut_dyn <- foldDyn ($) ([], aut, []) $ leftmost [trimPS' <$ evt, makeHomogeneousPS' <$ evt2, makeStandardPS' <$ evt3, renumStatesPS' <$ evt4, prec' <$ evt5, next' <$ evt6, switchInit' <$> evt7, switchFinal' <$> evt8, switchTrans' <$> evt9]
      evt <- labelledButton "trim" ((\(_x, y, _z) -> isNotTrimPS' y) <$> aut_dyn)
      evt2 <- labelledButton "makeHomogeneous" ((\(_x, y, _z) -> isNotHomogeneousPS' y) <$> aut_dyn)
      evt3 <- labelledButton "makeStandard" ((\(_x, y, _z) -> isNotStandardPS' y) <$> aut_dyn)
      evt4 <- labelledButton "renum" $ constDyn True
      evt5 <- labelledButton "<" ((\(x, _y, _z) -> not $ null x) <$> aut_dyn)
      evt6 <- labelledButton ">" ((\(_x, _y, z) -> not $ null z) <$> aut_dyn)
      evt7 <- lecteurInt "Switch initiality of a state" "Switch" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "initText"
      evt8 <- lecteurInt "Switch finality of a state" "Switch" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "finalText"
      evt9 <- lecteurTrans "Switch existence of a transition" "Switch" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "transText"
  _ <-
    dyn $
      theContent . (\(_x, y, _z) -> y)
        <$> aut_dyn
  _ <- dynText $ kosarajuStringPS' . (\(_x, y, _z) -> y) <$> aut_dyn
  footer
  where
    kosarajuStringPS' (Left aut) = pack $ show $ (pack . toString <$>) <$> kosaraju aut
    kosarajuStringPS' (Right aut) = pack $ show $ (pack <$>) <$> kosarajuStringPS aut
    switchInit' (Just i) (before, Left aut, _after) = (Left aut : before, Left $ switchInit i aut, [])
    switchInit' _m_int au = au
    switchFinal' (Just i) (before, Left aut, _after) = (Left aut : before, Left $ switchFinal i aut, [])
    switchFinal' _m_int au = au
    switchTrans' (Just t) (before, Left aut, _after) = (Left aut : before, Left $ switchTrans t aut, [])
    switchTrans' _m_int au = au
    isNotTrimPS' (Left aut) = not $ isTrim aut
    isNotTrimPS' (Right aut) = not $ isTrimPS aut
    isNotHomogeneousPS' (Left aut) = not $ isHomogeneous aut
    isNotHomogeneousPS' (Right aut) = not $ isHomogeneousPS aut
    isNotStandardPS' (Left aut) = not $ isStandard aut
    isNotStandardPS' (Right aut) = not $ isStandardPS aut
    trimPS' (before, Left aut, _after) = (Left aut : before, Left $ trim aut, [])
    trimPS' (before, Right aut, _after) = (Right aut : before, Right $ trimPS aut, [])
    makeHomogeneousPS' (before, Left aut, _after) = (Left aut : before, Right $ makeHomogeneousPS $ PSNFA aut, [])
    makeHomogeneousPS' (before, Right aut, _after) = (Right aut : before, Right $ makeHomogeneousPS aut, [])
    makeStandardPS' (before, Left aut, _after) = (Left aut : before, Right $ makeStandardPS $ PSNFA aut, [])
    makeStandardPS' (before, Right aut, _after) = (Right aut : before, Right $ makeStandardPS aut, [])
    renumStatesPS' (before, Left aut, _after) = (Left aut : before, Left $ renumStates aut, [])
    renumStatesPS' (before, Right aut, _after) = (Right aut : before, Left $ renumStatesPStoNFA aut, [])
    prec' (before, aut, after) = case before of
      [] -> (before, aut, after)
      (x : xs) -> (xs, x, aut : after)
    next' (before, aut, after) = case after of
      [] -> (before, aut, after)
      (x : xs) -> (aut : before, x, xs)

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
