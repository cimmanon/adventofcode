module Day4 where

import Data.List (nub, sort)

countValid :: (String -> Bool) -> IO Int
countValid f = readFile "day4.txt" >>= return . length . filter f . lines

{----------------------------------------------------------------------------------------------------{
                                                                      | Puzzle 1
}----------------------------------------------------------------------------------------------------}

puzzle1 :: IO Int
puzzle1 = countValid noRepeatWords

noRepeatWords :: String -> Bool
noRepeatWords x = x == unwords (nub $ words x)

{----------------------------------------------------------------------------------------------------{
                                                                      | Puzzle 2
}----------------------------------------------------------------------------------------------------}

puzzle2 :: IO Int
puzzle2 = countValid noAnnagrams

noAnnagrams :: String -> Bool
noAnnagrams x =
	let
		sorted = map sort $ words x
	in
		sorted == nub sorted
