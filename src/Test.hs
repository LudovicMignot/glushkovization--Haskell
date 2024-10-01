module Test where

import qualified Data.Map as Map
import qualified Data.Set as Set
import NFA
  ( NFA (NFA),
    addTransitionInMap,
    generateHomogeneousNFA,
    generateNFA,
    generateNFASuchThat,
    generateStandardNFA,
    getUsefulStates,
    isHomogeneous,
    isStandard,
    kosaraju,
    kosaraju1,
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

runKosa1 :: IO ()
runKosa1 = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_kosa1" aut
  let theList = kosaraju1 aut
  putStrLn $ toString theList

runKosa1' :: IO ()
runKosa1' = do
  let trans = [(1, 'a', 2), (4, 'a', 1), (3, 'a', 4), (5, 'a', 3), (4, 'a', 5)]
  let aut = (NFA (Set.singleton 1) Set.empty (foldr (flip addTransitionInMap) Map.empty trans) :: NFA Char Int)
  _ <- toPngInImgDir "test_kosa1'" aut
  let theList = kosaraju1 aut
  putStrLn $ toString theList

runKosa :: IO ()
runKosa = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 10 :: Int] 3 3 25
  _ <- toPngInImgDir "test_kosa" aut
  let theList = kosaraju aut
  print theList