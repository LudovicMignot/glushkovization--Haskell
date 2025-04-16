{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module MainContent where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isLeft)
import Data.Foldable (forM_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import HistoNFA
  ( extIsol',
    intIsol',
    makeHomogeneousPS',
    makeStandardPS',
    newHisto,
    next',
    orbNFA,
    prec',
    renumStatesPS',
    stabOrbNFA,
    stabPS',
    switchFinals',
    switchInits',
    switchTranss',
    trimPS',
  )
import NFA (NFA, generateNFA, getPreds, getStates)
import NFAAccessibility (isTrim)
import NFAHomogeneity (isHomogeneous)
import NFAOrbit (ingates, isExtIsolated, isIntIsolated, isIsolatedNFA, isStable, isStableNFA, isStronglyStable, isStronglyStableNFA, kosaraju, outgates)
import NFAStandard (isStandard)
import PSNFA (PSNFA (PSNFA), isHomogeneousPS, isIsolatedNFAPS, isStableNFAPS, isStandardPS, isStronglyStableNFAPS, isTrimPS)
import Reflex (Dynamic, constDyn, ffor, performEvent)
import Reflex.Dom.Core (DomBuilder, MonadWidget, foldDyn, leftmost, (=:))
import Reflex.Dom.Widget (dyn, el, elAttr, text)
import ToString (toString)
import Widget (labelledButton, lecteurIntSuchThat, lecteurInts, lecteurTrans)

header :: (DomBuilder t m) => m ()
header =
  elAttr
    "link"
    (Map.fromList [("rel", "stylesheet"), ("href", bootstrapCSSCDN)])
    $ return ()
  where
    bootstrapCSSCDN =
      "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"

body ::
  (MonadWidget t m) =>
  ( Dynamic
      t
      ( [Either (NFA Char Int) (PSNFA Char)],
        Either (NFA Char Int) (PSNFA Char),
        [Either (NFA Char Int) (PSNFA Char)]
      ) ->
    m ()
  ) ->
  m ()
body wasm_content = do
  _ <- elAttr "div" ("class" =: "container") $ do
    elAttr "h1" ("class" =: "text-center") $ text "Orbit Strong Stabilization"
    el "hr" $ return ()
  aut <- liftIO $ Left <$> generateNFA ['a' .. 'e'] [(0 :: Int) .. 5] 2 2 10

  elAttr "div" ("class" =: "d-flex flex-row") $ do
    rec aut_dyn <- foldDyn ($) (newHisto aut) $ leftmost [trimPS' <$ evt, makeHomogeneousPS' <$ evt2, makeStandardPS' <$ evt3, renumStatesPS' <$ evt4, prec' <$ evt5, next' <$ evt6, switchInits' <$> evt7, switchFinals' <$> evt8, switchTranss' <$> evt9, extIsol' <$> evt10, intIsol' <$> evt11, (\new_aut (before, old, _after) -> (old : before, new_aut, [])) <$> evt12, orbNFA <$> evt13, stabOrbNFA <$> evt14, stabPS' <$ evt15]
        ((evt, evt2, evt3), (evt7, evt8, evt9, evt12), (evt10, evt11, evt13, evt14, evt15)) <- elAttr "div" (Map.fromList [("class", "w-25")]) $
          elAttr "div" (Map.fromList [("class", "accordion w-100 sticky-top"), ("id", "accordion_menu")]) $ do
            res_evt <-
              (,,)
                <$> ( elAttr "div" ("class" =: "accordion-item") $ do
                        elAttr "h2" ("class" =: "accordion-header") $
                          elAttr "button" (Map.fromList [("class", "accordion-button collapsed"), ("type", "button"), ("data-bs-toggle", "collapse"), ("data-bs-target", "#collapseOne")]) $
                            text "Commands"
                        elAttr "div" ("class" =: "d-flex justify-content-center") $
                          elAttr "div" (Map.fromList [("id", "collapseOne"), ("class", "accordion-collapse collapse")]) $
                            elAttr "div" (Map.fromList [("role", "group"), ("class", "btn-group m-2")]) $
                              (,,)
                                <$> labelledButton "Trim" ((\(_x, y, _z) -> either (not . isTrim) (not . isTrimPS) y) <$> aut_dyn)
                                <*> labelledButton "Homogenize + Trim" ((\(_x, y, _z) -> either (not . isHomogeneous) (not . isHomogeneousPS) y) <$> aut_dyn)
                                <*> labelledButton "Standardize + Trim" ((\(_x, y, _z) -> either (not . isStandard) (not . isStandardPS) y) <$> aut_dyn)
                    )
                <*> ( elAttr "div" ("class" =: "accordion-item") $ do
                        elAttr "h2" ("class" =: "accordion-header") $
                          elAttr "button" (Map.fromList [("class", "accordion-button collapsed"), ("type", "button"), ("data-bs-toggle", "collapse"), ("data-bs-target", "#collapseTwo")]) $
                            text "Construction"
                        elAttr "div" (Map.fromList [("id", "collapseTwo"), ("class", "accordion-collapse collapse")]) $ do
                          elAttr "div" ("class" =: "d-flex flex-column justify-content-center") $
                            (,,,)
                              <$> lecteurInts "Switch initiality of states" "Switch" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "initText"
                              <*> lecteurInts "Switch finality of a state" "Switch" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "finalText"
                              <*> lecteurTrans "Switch existence of a transition" "Switch" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "transText"
                              <*> do
                                evt_click <- labelledButton "⚅ Randomize ⚅" $ constDyn True
                                performEvent $
                                  ffor evt_click $
                                    const $
                                      (liftIO $ Left <$> generateNFA ['a' .. 'e'] [(0 :: Int) .. 5] 2 2 10)
                    )
                <*> ( elAttr "div" ("class" =: "accordion-item") $
                        do
                          elAttr "h2" ("class" =: "accordion-header") $
                            elAttr "button" (Map.fromList [("class", "accordion-button collapsed"), ("type", "button"), ("data-bs-toggle", "collapse"), ("data-bs-target", "#collapseThree")]) $
                              text "Orbital operations"
                          elAttr "div" (Map.fromList [("id", "collapseThree"), ("class", "accordion-collapse collapse")]) $ do
                            elAttr "div" ("class" =: "d-flex flex-column justify-content-center") $
                              (,,,,)
                                <$> ( lecteurIntSuchThat "Outgate isolation" "isolate" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "extIsol" $
                                        \i ->
                                          let isExtGate _ (Right _) = False
                                              isExtGate p (Left autom) = isTrim autom && isHomogeneous autom && isStandard autom && (any (\o -> let outs = outgates autom o in p `Set.member` outs && Set.size outs > 1) $ Set.fromList <$> kosaraju autom)
                                           in (isExtGate i . \(_x, y, _z) -> y) <$> aut_dyn
                                    )
                                <*> ( lecteurIntSuchThat "Ingate isolation" "isolate" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "intIsol" $
                                        \i ->
                                          let goodProp _ (Right _) = False
                                              goodProp p (Left autom) = isTrim autom && isHomogeneous autom && isStandard autom && (any (\o -> let ins = ingates autom o in p `Set.member` o && ins /= Set.singleton p) $ Set.fromList <$> kosaraju autom)
                                           in (goodProp i . \(_x, y, _z) -> y) <$> aut_dyn
                                    )
                                <*> ( lecteurIntSuchThat "Orbital NFA" "compute" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "orbNFA" $
                                        \i ->
                                          let isIntStateWithPred _ (Right _) = False
                                              isIntStateWithPred p (Left autom) = isTrim autom && isHomogeneous autom && isStandard autom && not (Set.null (getPreds autom p)) && (any (\o -> let ins = ingates autom o in p `Set.member` o && not (Set.null ins)) $ Set.fromList <$> kosaraju autom)
                                           in (isIntStateWithPred i . \(_x, y, _z) -> y) <$> aut_dyn
                                    )
                                <*> ( lecteurIntSuchThat "Stabilizes orbit" "compute" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "stabOrb" $
                                        \i ->
                                          let isInIsolatedOrbWithPred _ (Right _) = False
                                              isInIsolatedOrbWithPred p (Left autom) =
                                                let o = Set.unions $ filter (p `Set.member`) $ Set.fromList <$> kosaraju autom
                                                 in isTrim autom && isHomogeneous autom && isStandard autom && isExtIsolated autom o && isIntIsolated autom o && p `Set.member` getStates autom && not (Set.null (getPreds autom p))
                                           in (isInIsolatedOrbWithPred i . \(_x, y, _z) -> y) <$> aut_dyn
                                    )
                                <*> (labelledButton "Strongly Stabilize" ((\(_x, y, _z) -> either (\nfa -> isStandard nfa && isHomogeneous nfa && isTrim nfa && not (isStronglyStableNFA nfa)) (\nfa -> isStandardPS nfa && isHomogeneousPS nfa && isTrimPS nfa && not (isStronglyStableNFAPS nfa)) y) <$> aut_dyn))
                    )
            _ <-
              ( elAttr "div" ("class" =: "accordion-item") $ do
                  elAttr "h2" ("class" =: "accordion-header") $
                    elAttr "button" (Map.fromList [("class", "accordion-button collapsed"), ("type", "button"), ("data-bs-toggle", "collapse"), ("data-bs-target", "#collapseFour")]) $
                      text "Informations"
                  elAttr "div" ("class" =: "d-flex justify-content-center") $
                    elAttr "div" (Map.fromList [("id", "collapseFour"), ("class", "accordion-collapse collapse")]) $
                      dyn $
                        theInfos . (\(_x, y, _z) -> y) <$> aut_dyn
              )

            _ <-
              ( elAttr "div" ("class" =: "accordion-item") $ do
                  elAttr "h2" ("class" =: "accordion-header") $
                    elAttr "button" (Map.fromList [("class", "accordion-button collapsed"), ("type", "button"), ("data-bs-toggle", "collapse"), ("data-bs-target", "#collapseFive")]) $
                      text "Orbital Informations"
                  elAttr "div" ("class" =: "d-flex justify-content-center") $
                    elAttr "div" (Map.fromList [("id", "collapseFive"), ("class", "accordion-collapse collapse")]) $
                      dyn $
                        theOrbInfos . (\(_x, y, _z) -> y) <$> aut_dyn
              )
            return res_evt
        (evt4, evt5, evt6) <- elAttr "div" ("class" =: "d-flex flex-column w-75") $ do
          evts <-
            elAttr "div" (Map.fromList [("role", "group"), ("class", "btn-group sticky-top")]) $
              (,,)
                <$> (labelledButton "State Renumerotation" $ constDyn True)
                <*> labelledButton "<" ((\(x, _y, _z) -> not $ null x) <$> aut_dyn)
                <*> labelledButton ">" ((\(_x, _y, z) -> not $ null z) <$> aut_dyn)

          _ <- wasm_content aut_dyn

          return evts

        return ()
    return ()

  footer
  where
    theOrbInfos' aut = do
      elAttr "table" ("class" =: "table table-striped table-hover p-2") $ do
        elAttr "thead" ("class" =: "table-dark") $ do
          el "tr" $ do
            elAttr "th" ("scope" =: "col" <> "class" =: "text-center align-middle") $ text "Orbit"
            elAttr "th" ("scope" =: "col" <> "class" =: "text-center align-middle") $ text "Ingate Isolated"
            elAttr "th" ("scope" =: "col" <> "class" =: "text-center align-middle") $ text "Outgate Isolated"
            elAttr "th" ("scope" =: "col" <> "class" =: "text-center align-middle") $ text "Stable"
            elAttr "th" ("scope" =: "col" <> "class" =: "text-center align-middle") $ text "Strongly stable"
        el "tbody" $ do
          forM_ orbs $ \orbit -> do
            el "tr" $ do
              elAttr "td" ("class" =: "text-center") $ text $ pack (toString orbit)
              elAttr "td" ("class" =: "text-center") $ text $ pack (toString $ isIntIsolated aut orbit)
              elAttr "td" ("class" =: "text-center") $ text $ pack (toString $ isExtIsolated aut orbit)
              elAttr "td" ("class" =: "text-center") $ text $ pack (toString $ isStable aut orbit)
              elAttr "td" ("class" =: "text-center") $ text $ pack (toString $ isStronglyStable aut orbit)
      where
        orbs = Set.fromList <$> kosaraju aut
    theOrbInfos (Left aut) = theOrbInfos' aut
    theOrbInfos (Right (PSNFA nfa)) = theOrbInfos' nfa

    theInfos (Right psAut) = do
      elAttr "div" ("class" =: "d-flex flex-row justify-content-center") $ do
        el "p" $ text "The NFA is:"
        el "ul" $ do
          el "li" $ text $ "Trimmed: " <> pack (show $ isTrimPS psAut)
          el "li" $ text $ "Homogeneous: " <> pack (show $ isHomogeneousPS psAut)
          el "li" $ text $ "Standard: " <> pack (show $ isStandardPS psAut)
          el "li" $ text $ "Orbitally isolated: " <> pack (show $ isIsolatedNFAPS psAut)
          el "li" $ text $ "Stable: " <> pack (show $ isStableNFAPS psAut)
          el "li" $ text $ "Strongly stable: " <> pack (show $ isStronglyStableNFAPS psAut)
    theInfos (Left aut) = do
      elAttr "div" ("class" =: "d-flex flex-row justify-content-center") $ do
        el "p" $ text "The NFA is:"
        el "ul" $ do
          el "li" $ text $ "Trimmed: " <> pack (show $ isTrim aut)
          el "li" $ text $ "Homogeneous: " <> pack (show $ isHomogeneous aut)
          el "li" $ text $ "Standard: " <> pack (show $ isStandard aut)
          el "li" $ text $ "Orbitally isolated: " <> pack (show $ isIsolatedNFA aut)
          el "li" $ text $ "Stable: " <> pack (show $ isStableNFA aut)
          el "li" $ text $ "Strongly stable: " <> pack (show $ isStronglyStableNFA aut)

footer :: (DomBuilder t m) => m ()
footer = do
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", bootstrapJsCDN)]) $
    return ()
  elAttr "script" (Map.fromList [("defer", "defer"), ("src", jQueryCDN), ("crossorigin", "anonymous")]) $
    return ()
  where
    bootstrapJsCDN =
      "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"
    jQueryCDN = "https://code.jquery.com/jquery-3.7.1.js"
