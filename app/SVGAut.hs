{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SVGAut where

import Data.Text as Te
  ( pack,
  )
import GHC.Wasm.Prim
import PSNFA (PSNFA, nfaToDotPS)
import Reflex (constDyn)
import Reflex.Dom.Core
  ( MonadWidget,
    elDynHtmlAttr',
    (=:),
  )
import Reflex.Dom.Widget
  ( el,
  )
import ToString (ToString)

foreign import javascript unsafe "var im = Viz( $1 , { format: \"svg\" }); console.log(im); return im;"
  vizSVG :: JSString -> JSString

-- foreign import javascript unsafe "\
--   \var im = Viz($1, { format: 'svg' }); \
--   \var parser = new DOMParser(); \
--   \var svgDoc = parser.parseFromString(im, 'image/svg+xml'); \
--   \var svgElement = svgDoc.documentElement; \
--   \svgElement.setAttribute('flex-grow', '1'); \
--   \var serializer = new XMLSerializer(); \
--   \var svgWithoutDimensions = serializer.serializeToString(svgElement); \
--   \console.log(svgWithoutDimensions); \
--   \return svgWithoutDimensions;"
--   vizSVG :: JSString -> JSString

-- foreign import javascript unsafe "\
--   \var im = Viz($1, { format: 'svg' }); \
--   \var parser = new DOMParser(); \
--   \var svgDoc = parser.parseFromString(im, 'image/svg+xml'); \
--   \var svgElement = svgDoc.documentElement; \
--   \svgElement.removeAttribute('width'); \
--   \svgElement.removeAttribute('height'); \
--   \svgElement.setAttribute('preserveAspectRatio', 'xMidYMid meet'); \
--   \var serializer = new XMLSerializer(); \
--   \var svgWithoutDimensions = serializer.serializeToString(svgElement); \
--   \console.log(svgWithoutDimensions); \
--   \return svgWithoutDimensions;"
--   vizSVG :: JSString -> JSString

svgAut ::
  ( MonadWidget t m,
    ToString symbol
  ) =>
  PSNFA symbol ->
  m ()
svgAut auto = do
  _ <-
    elDynHtmlAttr' "figure" ("id" =: "svgaut-container" <> "class" =: "m-auto") $
      constDyn $
        Te.pack $
          fromJSString $
            vizSVG $
              toJSString $
                nfaToDotPS auto
  return ()

-- svgAut auto = do
--   let getData handle = do
--         bytes <- BS.hGetContents handle
--         return $ Te.pack $ toString bytes
--   svg <- liftIO $ graphvizWithHandle Dot (parseDotGraph $ Tel.pack $ nfaToDotPS auto :: G.DotGraph String) Svg getData
--   void $ elDynHtml' "div" $ return svg