module Test where

import NFA
import ToString ()

randomAut :: IO (NFA Char Int)
randomAut = generateNFA ['a' .. 'c'] [1 .. 7] 3 3 10

stdAut :: IO (NFA Char (Maybe Int))
stdAut = makeStandard <$> randomAut

a :: IO String
a = fmap nfaToDot randomAut

b :: IO FilePath
b = randomAut >>= toPngInImgDir "test"

run :: IO FilePath
run = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test" aut
  toPngInImgDir "test_std" $ makeStandard aut
