{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Widget where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import Data.GraphViz.Commands
  ( GraphvizCommand (Dot),
    GraphvizOutput (Svg),
    graphvizWithHandle,
  )
import Data.GraphViz.Types (parseDotGraph)
import Data.GraphViz.Types.Generalised as G (DotGraph)
import Data.Text as Te
  ( Text,
    pack,
  )
import qualified Data.Text.Lazy as Tel
import NFA (NFA)
import NFADotRepr (nfaToDot)
import Reflex.Dom.Core
  ( EventName (Click),
    MonadWidget,
    Reflex (Event),
    domEvent,
    elDynHtml',
    (=:),
  )
import Reflex.Dom.Widget
import ToString (ToString (toString))

svgAut ::
  ( MonadWidget t m,
    ToString symbol,
    ToString state,
    Ord state
  ) =>
  NFA symbol state ->
  m ()
svgAut auto = do
  let getData handle = do
        bytes <- BS.hGetContents handle
        return $ Te.pack $ toString bytes
  svg <- liftIO $ graphvizWithHandle Dot (parseDotGraph $ Tel.pack $ nfaToDot auto :: G.DotGraph String) Svg getData
  void $ elDynHtml' "div" $ return svg

labelledButton :: (MonadWidget t m) => Text -> m (Event t ())
labelledButton lbl = do
  (e, _) <- elAttr' "div" ("class" =: "btn btn-primary mx-1") $ text lbl
  return $ () <$ domEvent Click e