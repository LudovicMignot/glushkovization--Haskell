{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Widget where

import Control.Monad (void)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text as Te
  ( Text,
    unpack,
  )
import Data.Void
import Reflex (Dynamic, constDyn, ffor2, ffor3, tagPromptlyDyn)
import Reflex.Dom.Core
  ( EventName (Click),
    MonadWidget,
    Reflex (Event),
    domEvent,
    (=:),
  )
import Reflex.Dom.Widget
  ( TextInput (_textInput_value),
    def,
    el,
    elAttr,
    elDynAttr',
    text,
    textInput,
    textInputConfig_attributes,
    textInputConfig_initialValue,
    textInputConfig_inputType,
    (&),
    (.~),
  )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)

type Parser = Parsec Void String

-- Parser pour les entiers
integer :: Parser Int
integer = read <$> (optional separator *> some digitChar <* optional separator)

-- Parser pour les caractères séparateurs (espaces, tabulations, virgules, points-virgules)
separator :: Parser ()
separator = void $ many (oneOf [',', ';'] <|> spaceChar)

-- Parser pour une liste d'entiers séparés par des caractères non entiers
integerList :: Parser [Int]
integerList = integer `sepBy` separator <* eof

-- Fonction pour tester le parser
parseIntegerList :: String -> Maybe [Int]
parseIntegerList = eitherToMaybe . parse integerList ""
  where
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right []) = Nothing
    eitherToMaybe (Right a) = Just a

readIntsSuchThat ::
  (MonadWidget t m) =>
  Text ->
  Text ->
  ([Int] -> Dynamic t Bool) ->
  m (Dynamic t (Maybe [Int]))
readIntsSuchThat ident start tester = do
  let errorState =
        Map.fromList [("class", "form-control is-invalid"), ("id", ident)]
      validState =
        Map.fromList [("class", "form-control is-valid"), ("id", ident)]
  rec n <-
        textInput $
          def
            & textInputConfig_inputType
            .~ "text"
            & textInputConfig_initialValue
            .~ start
            & textInputConfig_attributes
            .~ attrs

      let result = parseIntegerList . Te.unpack <$> _textInput_value n
          attrs =
            result
              >>= ( \m_int ->
                      case m_int of
                        Nothing -> constDyn errorState
                        Just is -> tester is >>= \b -> if b then constDyn validState else constDyn errorState
                  )
  return result

labelledButton :: (MonadWidget t m) => Text -> Dynamic t Bool -> m (Event t ())
labelledButton lbl isActive = do
  (e, _) <-
    elDynAttr' "div" ((\b -> "class" =: ("btn btn-primary align-content-center" <> active b)) <$> isActive) $ text lbl
  return $ () <$ domEvent Click e
  where
    active isActive' = if isActive' then "" else " disabled"

readInput ::
  (MonadWidget t m, Read a) =>
  Text ->
  Text ->
  m (Dynamic t (Maybe a))
readInput ident start = do
  let errorState =
        Map.fromList [("class", "form-control is-invalid"), ("id", ident)]
      validState =
        Map.fromList [("class", "form-control is-valid"), ("id", ident)]
  rec n <-
        textInput $
          def
            & textInputConfig_inputType
            .~ "text"
            & textInputConfig_initialValue
            .~ start
            & textInputConfig_attributes
            .~ attrs

      let result = readMaybe . Te.unpack <$> _textInput_value n
          attrs =
            ( \m_int ->
                case m_int of
                  Nothing -> errorState
                  Just _ -> validState
            )
              <$> result
  return result

readIntSuchThat ::
  (MonadWidget t m) =>
  Text ->
  Text ->
  (Int -> Dynamic t Bool) ->
  m (Dynamic t (Maybe Int))
readIntSuchThat ident start tester = do
  let errorState =
        Map.fromList [("class", "form-control is-invalid"), ("id", ident)]
      validState =
        Map.fromList [("class", "form-control is-valid"), ("id", ident)]
  rec n <-
        textInput $
          def
            & textInputConfig_inputType
            .~ "text"
            & textInputConfig_initialValue
            .~ start
            & textInputConfig_attributes
            .~ attrs

      let result = readMaybe . Te.unpack <$> _textInput_value n
          attrs =
            result
              >>= ( \m_int ->
                      case m_int of
                        Nothing -> constDyn errorState
                        Just i -> tester i >>= \b -> if b then constDyn validState else constDyn errorState
                  )
  return result

lecteurInt :: (MonadWidget t m) => Text -> Text -> Dynamic t Bool -> Text -> m (Event t (Maybe Int))
lecteurInt lbl lbl_but isActive ident = lecteurIntSuchThat lbl lbl_but isActive ident (constDyn . const True)

lecteurIntSuchThat :: (MonadWidget t m) => Text -> Text -> Dynamic t Bool -> Text -> (Int -> Dynamic t Bool) -> m (Event t (Maybe Int))
lecteurIntSuchThat lbl lbl_but isActive ident tester = el "form" $
  elAttr "div" ("class" =: "form-group  my-2") $ do
    elAttr "label" ("for" =: ident) $ text lbl
    res <- elAttr "div" ("class" =: "input-group") $ do
      m_int <- readIntSuchThat "inputWord" "0" tester
      evt <- labelledButton lbl_but $ ffor3 (m_int >>= maybe (constDyn False) tester) isActive m_int (\b b' m_int_val -> b && b' && isJust m_int_val)
      return $ tagPromptlyDyn m_int evt
    elAttr "small" ("class" =: "form-text text-muted") $
      text "Enter an integer, made of the symbols in [0 .. 9]."
    return res

lecteurInts :: (MonadWidget t m) => Text -> Text -> Dynamic t Bool -> Text -> m (Event t (Maybe [Int]))
lecteurInts lbl lbl_but isActive ident = lecteurIntsSuchThat lbl lbl_but isActive ident (constDyn . const True)

lecteurIntsSuchThat :: (MonadWidget t m) => Text -> Text -> Dynamic t Bool -> Text -> ([Int] -> Dynamic t Bool) -> m (Event t (Maybe [Int]))
lecteurIntsSuchThat lbl lbl_but isActive ident tester = el "form" $
  elAttr "div" ("class" =: "form-group  my-2") $ do
    elAttr "label" ("for" =: ident) $ text lbl
    res <- elAttr "div" ("class" =: "input-group") $ do
      m_int <- readIntsSuchThat "inputWord" "0" tester
      evt <- labelledButton lbl_but $ ffor3 (m_int >>= maybe (constDyn False) tester) isActive m_int (\b b' m_int_val -> b && b' && isJust m_int_val)
      return $ tagPromptlyDyn m_int evt
    elAttr "small" ("class" =: "form-text text-muted") $
      text "Enter a list integers, made of the symbols in [0 .. 9]."
    return res

lecteurTrans :: (MonadWidget t m) => Text -> Text -> Dynamic t Bool -> Text -> m (Event t (Maybe (Int, Char, Int)))
lecteurTrans lbl lbl_but isActive ident = el "form" $
  elAttr "div" ("class" =: "form-group") $ do
    elAttr "label" ("for" =: ident) $ text lbl
    res <- elAttr "div" ("class" =: "input-group") $ do
      m_int <- readInput "inputWord" "(0, 'a', 1)"
      evt <- labelledButton lbl_but $ ffor2 isActive m_int (\b m_int_val -> b && isJust m_int_val)
      return $ tagPromptlyDyn m_int evt
    elAttr "small" ("class" =: "form-text text-muted") $
      text "Enter a transition, a triple (p, l, q) where p and q are made of the symbols in [0 .. 9] and l a quoted (') character."
    return res