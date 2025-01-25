{-# LANGUAGE FlexibleInstances #-}

module ToString where

import Control.Monad.Free (Free (Free, Pure))
import Data.Char (toUpper, chr)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import MonoEither (FreeEither, MonoEither (MonoEither))
import qualified Data.ByteString as BS

-- | String conversion from a type
class ToString a where
  -- | Conversion into an HTML String
  toHtmlString :: a -> String
  toHtmlString = toString

  -- | Upper case conversion for the symbols between "<" and ">"
  toHtmlCapString :: a -> String
  toHtmlCapString x = change $ toHtmlString x
    where
      change "" = ""
      change ('<' : l) = '<' : change' l
      change (y : l) = y : change l
      change' "" = ""
      change' ('>' : l) = '>' : change l
      change' (y : l) = toUpper y : change' l

  -- | String conversion
  toString :: a -> String

  -- | String printing
  myPrintLn :: a -> IO ()
  myPrintLn x = putStrLn $ toString x

instance ToString BS.ByteString where
  toString bs = chr . fromEnum <$> BS.unpack bs

instance ToString Bool where
  toString True = "true"
  toString False = "false"

instance ToString () where
  toString _ = "\"( )\""

instance ToString Char where
  toString a = [a]

instance ToString Int where
  toString = show

instance ToString Word where
  toString = show

instance (ToString a) => ToString (Set a) where
  toString s
    | Set.null s = "∅"
    | otherwise = "{" ++ intercalate "," (map toString $ Set.toList s) ++ "}"

  toHtmlString s
    | Set.null s = "∅"
    | otherwise = "{" ++ intercalate "," (map toHtmlString $ Set.toList s) ++ "}"

instance (ToString a, ToString b) => ToString (a, b) where
  toString (x, y) = "(" ++ toString x ++ "," ++ toString y ++ ")"

  toHtmlString (x, y) = "(" ++ toHtmlString x ++ "," ++ toHtmlString y ++ ")"

instance (ToString a, ToString b, ToString c) => ToString (a, b, c) where
  toString (x, y, z) = "(" ++ toString x ++ "," ++ toString y ++ "," ++ toString z ++ ")"

  toHtmlString (x, y, z) = "(" ++ toHtmlString x ++ "," ++ toHtmlString y ++ "," ++ toHtmlString z ++ ")"

instance (ToString a) => ToString [a] where
  toString l = intercalate "," $ map toString l

  toHtmlString l = intercalate "," $ map toHtmlString l

instance (ToString a) => ToString (Maybe a) where
  toString Nothing = "⊥"
  toString (Just x) = "J " ++ toString x

instance (ToString a, ToString b) => ToString (Either a b) where
  toString (Left a) = "Left " ++ toString a
  toString (Right b) = "Right " ++ toString b

instance (ToString a) => ToString (MonoEither a) where
  toString (MonoEither a) = toString a

instance (ToString a) => ToString (FreeEither a) where
  toString (Pure a) = toString a
  toString (Free b) = toString b