{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm
import MainContent (body, header)
import NFA (NFA)
import PSNFA (PSNFA (PSNFA))
import Reflex.Dom.Core (MonadWidget, mainWidgetWithHead, (=:))
import Reflex.Dom.Widget (dyn, elAttr)
import SVGAut (svgAut)

foreign export javascript "hs_start" hs_start :: IO ()

main :: IO ()
main = error "not necessary"

hs_start :: IO ()
hs_start =
  JSaddle.Wasm.run $
    mainWidgetWithHead
      header
      ( body
          ( \aut_dyn -> do
              _ <-
                dyn $
                  theContent . (\(_x, y, _z) -> y)
                    <$> aut_dyn
              return ()
          )
      )

theContent ::
  ( MonadWidget t m
  ) =>
  Either (NFA Char Int) (PSNFA Char) ->
  m ()
theContent aut =
  elAttr
    "div"
    ("class" =: "d-flex flex-column align-items-stretch")
    $ elAttr "div" ("class" =: "img-fluid")
    $ svgAut
    $ toPSNFA aut
  where
    toPSNFA (Right auto) = auto
    toPSNFA (Left auto) = PSNFA auto
