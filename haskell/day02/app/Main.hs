{-# LANGUAGE DerivingStrategies #-}

module Main where

import Data.Maybe (fromJust)

type Horizonal = Int

type Depth = Int

type Aim = Int

data Move = Forward Int | Up Int | Down Int
  deriving stock (Eq, Show)

makeMoveConstructorFromString :: String -> Maybe (Int -> Move)
makeMoveConstructorFromString "forward" = Just Forward
makeMoveConstructorFromString "down" = Just Down
makeMoveConstructorFromString "up" = Just Up
makeMoveConstructorFromString _ = Nothing

parseInputFile :: FilePath -> IO (Maybe [Move])
parseInputFile path = do
  content <- readFile path
  let substrings :: [(String, String)]
      substrings = map ((\ws -> (ws !! 0, ws !! 1)) . words) (lines content)
  return $ traverse (\(s, c) -> makeMoveConstructorFromString s <*> pure (read c)) substrings

calculateHorizontalPosition :: [Move] -> Horizonal
calculateHorizontalPosition [] = 0
calculateHorizontalPosition (Forward x : ms) = x + calculateHorizontalPosition ms
calculateHorizontalPosition (_ : ms) = 0 + calculateHorizontalPosition ms

calculateDepthPosition :: [Move] -> Depth
calculateDepthPosition [] = 0
calculateDepthPosition (Forward _ : ms) = calculateDepthPosition ms
calculateDepthPosition (Up x : ms) = calculateDepthPosition ms - x
calculateDepthPosition (Down x : ms) = x + calculateDepthPosition ms

calculateWithAim :: (Horizonal, Depth, Aim) -> [Move] -> Int
calculateWithAim (horizontal, depth, _) [] = horizontal * depth
calculateWithAim (horizontal, depth, aim) (Forward x : ms) = calculateWithAim (horizontal + x, depth + aim * x, aim) ms
calculateWithAim (horizontal, depth, aim) (Up x : ms) = calculateWithAim (horizontal, depth, aim - x) ms
calculateWithAim (horizontal, depth, aim) (Down x : ms) = calculateWithAim (horizontal, depth, aim + x) ms

main :: IO ()
main = do
  moves <- parseInputFile "input.txt"
  let moves' = fromJust moves
  putStrLn $ "Answer 1: " <> show (calculateHorizontalPosition moves' * calculateDepthPosition moves')
  putStrLn $ "Answer 2: " <> show (calculateWithAim (0, 0, 0) moves')
