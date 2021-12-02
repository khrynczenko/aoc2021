module Main where

import System.IO as IO

type DepthMeasurement = Word

parseInputFile :: FilePath -> IO [Word]
parseInputFile path = do
    contents <- IO.readFile path
    return $ map read $ lines contents

countIncreases :: [DepthMeasurement] -> Word
countIncreases (x1:x2:xs) =
    if x2 > x1
    then 1 + recurse
    else recurse
  where
    recurse = countIncreases (x2:xs)
countIncreases _ = 0

countIncreasesTriple :: [DepthMeasurement] -> Word
countIncreasesTriple (x1:x2:x3:x4:xs) =
    if x4 + x3 + x2 > x3 + x2 + x1
    then 1 + recurse
    else recurse
  where
    recurse = countIncreasesTriple (x2:x3:x4:xs)
countIncreasesTriple _ = 0

main :: IO ()
main = do
    measurements <- parseInputFile "input.txt"
    let depthIncreasesCount = countIncreases measurements
    putStrLn $ "Answer 1: " <> show depthIncreasesCount
    let depthIncreasesTripleCount = countIncreasesTriple measurements
    putStrLn $ "Answer 2: " <> show depthIncreasesTripleCount
