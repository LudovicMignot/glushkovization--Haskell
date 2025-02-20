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

svgAut ::
  ( MonadWidget t m,
    ToString symbol
  ) =>
  PSNFA symbol ->
  m ()
svgAut auto = do
  _ <-
    el "figure" $
      elDynHtmlAttr' "div" ("id" =: "svgaut-container") $
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