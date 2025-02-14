{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isLeft)
import Data.Foldable (forM_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Language.Javascript.JSaddle.Warp (run)
import NFA (NFA, generateNFA, getPreds, getStates, renumStates, switchFinal, switchInit, switchTrans)
import NFAAccessibility (isTrim, trim)
import NFAHomogeneity (isHomogeneous)
import NFAOrbit (externalIsolation, ingates, internalIsolation, isExtIsolated, isIntIsolated, isIsolatedNFA, isStable, isStableNFA, isStronglyStable, isStronglyStableNFA, kosaraju, kosarajuSet, orbitalNFA, outgates, stabilizeOrbit)
import NFAStandard (isStandard)
import PSNFA (PSNFA (PSNFA), isHomogeneousPS, isIsolatedNFAPS, isStableNFAPS, isStandardPS, isStronglyStableNFAPS, isTrimPS, kosarajuStringPS, makeHomogeneousPS, makeStandardPS, renumStatesPStoNFA, trimPS)
import Reflex (constDyn, ffor, performEvent)
import Reflex.Dom.Core (DomBuilder, MonadWidget, dynText, foldDyn, leftmost, mainWidgetWithHead, (=:))
import Reflex.Dom.Widget (dyn, el, elAttr, text)
import ToString (toString)
import Widget (labelledButton, lecteurInt, lecteurIntSuchThat, lecteurTrans, svgAut)

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
    elAttr "h1" ("class" =: "text-center") $ text "Orbit Stabilization"
    el "hr" $ return ()
  aut <- liftIO $ Left <$> generateNFA ['a' .. 'e'] [(0 :: Int) .. 5] 2 2 10

  elAttr "div" ("class" =: "d-flex flex-row") $ do
    rec aut_dyn <- foldDyn ($) ([], aut, []) $ leftmost [trimPS' <$ evt, makeHomogeneousPS' <$ evt2, makeStandardPS' <$ evt3, renumStatesPS' <$ evt4, prec' <$ evt5, next' <$ evt6, switchInit' <$> evt7, switchFinal' <$> evt8, switchTrans' <$> evt9, extIsol' <$> evt10, intIsol' <$> evt11, (\new_aut (before, old, _after) -> (old : before, new_aut, [])) <$> evt12, orbNFA <$> evt13, stabOrbNFA <$> evt14]
        ((evt, evt2, evt3), (evt7, evt8, evt9, evt12), (evt10, evt11, evt13, evt14)) <-
          elAttr "div" (Map.fromList [("class", "accordion w-25"), ("id", "accordion_menu")]) $ do
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
                                <$> labelledButton "Trim" ((\(_x, y, _z) -> isNotTrimPS' y) <$> aut_dyn)
                                <*> labelledButton "Homogenize + Trim" ((\(_x, y, _z) -> isNotHomogeneousPS' y) <$> aut_dyn)
                                <*> labelledButton "Standardize + Trim" ((\(_x, y, _z) -> isNotStandardPS' y) <$> aut_dyn)
                    )
                <*> ( elAttr "div" ("class" =: "accordion-item") $ do
                        elAttr "h2" ("class" =: "accordion-header") $
                          elAttr "button" (Map.fromList [("class", "accordion-button collapsed"), ("type", "button"), ("data-bs-toggle", "collapse"), ("data-bs-target", "#collapseTwo")]) $
                            text "Construction"
                        elAttr "div" (Map.fromList [("id", "collapseTwo"), ("class", "accordion-collapse collapse")]) $ do
                          elAttr "div" ("class" =: "d-flex flex-column justify-content-center") $
                            (,,,)
                              <$> lecteurInt "Switch initiality of a state" "Switch" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "initText"
                              <*> lecteurInt "Switch finality of a state" "Switch" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "finalText"
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
                          elAttr "div" (Map.fromList [("id", "collapseThree"), ("class", "accordion-collapse collapse")]) $
                            (,,,)
                              <$> ( lecteurIntSuchThat "External isolation" "isolate" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "extIsol" $
                                      \i ->
                                        let isExtGate _ (Right _) = False
                                            isExtGate p (Left autom) = isTrim autom && isHomogeneous autom && isStandard autom && (any (\o -> let outs = outgates autom o in p `Set.member` outs && Set.size outs > 1) $ Set.fromList <$> kosaraju autom)
                                         in (isExtGate i . \(_x, y, _z) -> y) <$> aut_dyn
                                  )
                              <*> ( lecteurIntSuchThat "Internal isolation" "isolate" ((\(_x, y, _z) -> isLeft y) <$> aut_dyn) "intIsol" $
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
            elAttr "div" (Map.fromList [("role", "group"), ("class", "btn-group")]) $
              (,,)
                <$> (labelledButton "renum" $ constDyn True)
                <*> labelledButton "<" ((\(x, _y, _z) -> not $ null x) <$> aut_dyn)
                <*> labelledButton ">" ((\(_x, _y, z) -> not $ null z) <$> aut_dyn)

          _ <-
            dyn $
              theContent . (\(_x, y, _z) -> y)
                <$> aut_dyn

          return evts

        return ()
    return ()

  -- _ <- dynText $ kosarajuStringPS' . (\(_x, y, _z) -> y) <$> aut_dyn
  footer
  where
    theOrbInfos (Left aut) = do
      el "table" $ do
        el "thead" $ do
          el "tr" $ do
            el "th" $ text "Orbit"
            el "th" $ text "Int Isolated"
            el "th" $ text "Ext Isolated"
            el "th" $ text "Stable"
            el "th" $ text "Strongly stable"
        el "tbody" $ do
          forM_ orbs $ \orbit -> do
            el "tr" $ do
              el "td" $ text $ pack (toString orbit)
              el "td" $ text $ pack (toString $ isIntIsolated aut orbit)
              el "td" $ text $ pack (toString $ isExtIsolated aut orbit)
              el "td" $ text $ pack (toString $ isStable aut orbit)
              el "td" $ text $ pack (toString $ isStronglyStable aut orbit)
      where
        orbs = Set.fromList <$> kosaraju aut
    theOrbInfos (Right (PSNFA nfa)) = do
      el "table" $ do
        el "thead" $ do
          el "tr" $ do
            el "th" $ text "Orbit"
            el "th" $ text "Int Isolated"
            el "th" $ text "Ext Isolated"
            el "th" $ text "Stable"
            el "th" $ text "Strongly stable"
        el "tbody" $ do
          forM_ orbs $ \orbit -> do
            el "tr" $ do
              el "td" $ text $ pack (toString orbit)
              el "td" $ text $ pack (toString $ isIntIsolated nfa orbit)
              el "td" $ text $ pack (toString $ isExtIsolated nfa orbit)
              el "td" $ text $ pack (toString $ isStable nfa orbit)
              el "td" $ text $ pack (toString $ isStronglyStable nfa orbit)
      where
        orbs = Set.fromList <$> kosaraju nfa

    theInfos (Right psAut) = do
      el "p" $ text "The NFA is:"
      el "ul" $ do
        el "li" $ text $ "Trimmed: " <> pack (show $ isTrimPS psAut)
        el "li" $ text $ "Homogeneous: " <> pack (show $ isHomogeneousPS psAut)
        el "li" $ text $ "Standard: " <> pack (show $ isStandardPS psAut)
        el "li" $ text $ "Orbitally isolated: " <> pack (show $ isIsolatedNFAPS psAut)
        el "li" $ text $ "Stable: " <> pack (show $ isStableNFAPS psAut)
        el "li" $ text $ "Strongly stable: " <> pack (show $ isStronglyStableNFAPS psAut)
    theInfos (Left aut) = do
      el "p" $ text "The NFA is:"
      el "ul" $ do
        el "li" $ text $ "Trimmed: " <> pack (show $ isTrim aut)
        el "li" $ text $ "Homogeneous: " <> pack (show $ isHomogeneous aut)
        el "li" $ text $ "Standard: " <> pack (show $ isStandard aut)
        el "li" $ text $ "Orbitally isolated: " <> pack (show $ isIsolatedNFA aut)
        el "li" $ text $ "Stable: " <> pack (show $ isStableNFA aut)
        el "li" $ text $ "Strongly stable: " <> pack (show $ isStronglyStableNFA aut)

    stabOrbNFA (Just i) (before, Left aut, _after) = (Left aut : before, Right $ PSNFA $ stabilizeOrbit aut orbit_i, [])
      where
        orbit_i = Set.unions $ Set.filter (i `Set.member`) $ kosarajuSet aut
    stabOrbNFA _m_int au = au
    orbNFA (Just i) (before, Left aut, _after) = (Left aut : before, Right $ PSNFA $ orbitalNFA aut orbit_i, [])
      where
        orbit_i = Set.unions $ Set.filter (i `Set.member`) $ kosarajuSet aut
    orbNFA _m_int au = au
    extIsol' (Just i) (before, Left aut, _after)
      | i `Set.member` (getStates aut `Set.intersection` outgates aut orbit_i) = (Left aut : before, Right $ PSNFA $ externalIsolation aut orbit_i i, [])
      where
        orbit_i = Set.unions $ Set.filter (i `Set.member`) $ kosarajuSet aut
    extIsol' _m_int au = au
    intIsol' (Just i) (before, Left aut, _after)
      | i `Set.member` (getStates aut) = (Left aut : before, Right $ PSNFA $ fst $ internalIsolation aut orbit_i i, [])
      where
        orbit_i = Set.unions $ Set.filter (i `Set.member`) $ kosarajuSet aut
    intIsol' _m_int au = au
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
    makeHomogeneousPS' (before, Left aut, _after) = (Left aut : before, Right $ trimPS . makeHomogeneousPS $ PSNFA aut, [])
    makeHomogeneousPS' (before, Right aut, _after) = (Right aut : before, Right $ trimPS $ makeHomogeneousPS aut, [])
    makeStandardPS' (before, Left aut, _after) = (Left aut : before, Right $ trimPS . makeStandardPS $ PSNFA aut, [])
    makeStandardPS' (before, Right aut, _after) = (Right aut : before, Right $ trimPS $ makeStandardPS aut, [])
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
    $ elAttr "div" ("class" =: "img-fluid")
    $ svgAut
    $ toPSNFA aut
  where
    toPSNFA (Right auto) = auto
    toPSNFA (Left auto) = PSNFA auto
