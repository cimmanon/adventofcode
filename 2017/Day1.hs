module Day1 where

import Data.Char (digitToInt)
import Data.List (genericLength)

{----------------------------------------------------------------------------------------------------{
                                                                      | Puzzle 1
}----------------------------------------------------------------------------------------------------}

sequential :: String -> Int
sequential xs = sumAdjacent 0 $ map digitToInt $ take (1 + length xs) $ cycle xs

sumAdjacent :: Int -> [Int] -> Int
sumAdjacent counter (x:y:ys) = sumAdjacent (counter + (if x == y then x else 0)) $ y : ys
sumAdjacent x _ = x

{----------------------------------------------------------------------------------------------------{
                                                                      | Puzzle 2
}----------------------------------------------------------------------------------------------------}

sumMid :: String -> Int
sumMid xs =
	let
		ys = map digitToInt xs
		midPoint = (genericLength ys) `div` 2
	in sum $ map (\(i, y) -> if (ys !! ((i + midPoint) `mod` genericLength ys)) == y then y else 0) $ zip [0..] ys
