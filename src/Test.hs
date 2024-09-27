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

runHomogeneous :: IO FilePath
runHomogeneous = do
  aut <- generateHomogeneousNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  toPngInImgDir "test_hom" aut

runStandard :: IO FilePath
runStandard = do
  aut <- generateStandardNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  toPngInImgDir "test_standard" aut

runStandardAndHomogeneous :: IO FilePath
runStandardAndHomogeneous = do
  aut <- generateNFASuchThat (\auto -> isStandard auto && isHomogeneous auto) ['a' .. 'c'] [1 .. 10 :: Int] 3 3 15
  toPngInImgDir "test_standard_hom" aut

runTestHomogeneous :: IO FilePath
runTestHomogeneous = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_hom_start" aut
  toPngInImgDir "test_hom_result" $ makeHomogeneous aut