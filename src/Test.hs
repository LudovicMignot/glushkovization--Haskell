module Test where

import NFA
  ( NFA,
    generateHomogeneousNFA,
    generateNFA,
    generateNFASuchThat,
    generateStandardNFA,
    getUsefulStates,
    isHomogeneous,
    isStandard,
    makeHomogeneous,
    makeStandard,
    nfaToDot,
    reverse,
    toPngInImgDir,
    trim,
  )
import ToString (toString)

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

runTestReversal :: IO FilePath
runTestReversal = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_rev_start" aut
  toPngInImgDir "test_rev_result" $ NFA.reverse aut

runUsefulTest :: IO ()
runUsefulTest = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_useful" aut
  let us = getUsefulStates aut
  putStrLn $ toString us

runTrimTest :: IO ()
runTrimTest = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_trim_start" aut
  _ <- toPngInImgDir "test_trim_result" $ trim aut
  let us = getUsefulStates aut
  putStrLn $ toString us
